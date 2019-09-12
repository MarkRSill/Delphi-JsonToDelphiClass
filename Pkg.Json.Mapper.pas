unit Pkg.Json.Mapper;

interface

uses
  FMX.TreeView, System.JSON, Rest.Json, RTTI, RegularExpressions, TypInfo,
  SysUtils, classes, Generics.Collections, Generics.Defaults;

type
  TJsonLibrary = (jlDelphi, jlGrijjy, jlXSuper);

  EJsonMapper = class(Exception);

  TJsonType = (jtUnknown, jtObject, jtArray, jtString, jtTrue, jtFalse, jtInteger, jtInt64, jtExtended,
    jtUnixTimestamp, jtDate, jtDateTime, jtBytes);

  TStubClass = class;

  TPkgJsonMapper = class;

  TStubField = class
  private
    FName: string;
    FPropertyName: string;
    FFieldName: string;
    FFieldType: TJsonType;
    FParentClass: TStubClass;
    procedure SetName(const Value: string);
    function GetCorrectedName: string;
    function GetAddNameAttrib: boolean;
  public
    constructor Create(AParentClass: TStubClass; AItemName: string; AFieldType: TJsonType);
    destructor Destroy; override;
    function GetTypeAsString: string; overload; virtual;
    class function GetTypeAsString(AType: TJsonType): string; overload;

    property Name: string read FName write SetName;
    property CorrectedName: string read GetCorrectedName;
    property FieldName: string read FFieldName write FFieldName;
    property PropertyName: string read FPropertyName write FPropertyName;
    property FieldType: TJsonType read FFieldType write FFieldType;
    property AddNameAttrib: boolean read GetAddNameAttrib;
  end;

  TStubContainerField = class(TStubField)
  private
    FFieldClass: TStubClass;
    FContainedType: TJsonType;
  public
    property ContainedType: TJsonType read FContainedType write FContainedType;
    property FieldClass: TStubClass read FFieldClass write FFieldClass;
  end;

  TStubObjectField = class(TStubContainerField)
  private
  public
    constructor Create(AParentClass: TStubClass; AItemName: string; AItemClass: TStubClass);
    function GetTypeAsString: string; override;
  end;

  TStubArrayField = class(TStubContainerField)
  private
  public
    constructor Create(AClass: TStubClass; AItemName: string; AItemSubType: TJsonType; AItemClass: TStubClass);
    function GetTypeAsString: string; override;
  end;

  TStubClass = class
  private
    FItems,
    FComplexItems,
    FArrayItems: TList<TStubField>;
    FName: string;
    FComparison: TComparison<TStubField>;
    FComparer: IComparer<TStubField>;
    FParentClass: TStubClass;
    FMapper: TPkgJsonMapper;
    FPureClassName: string;
    procedure SortFields;
    procedure SetName(const Value: string);
    procedure SetPureClassName(const Value: string);
    function GetCorrectedName: string;
  public
    constructor Create(AParentClass: TStubClass; AClassName: string; AMapper: TPkgJsonMapper);
    destructor Destroy; override;
    function GetDeclarationPart(IncludeSerializers: Boolean; JsonLibrary: TJsonLibrary): string;
    function GetImplementationPart(AJsonLibrary: TJsonLibrary; IncludeSerializers: Boolean = False): string;
    function FindField(AFieldName: string): TStubField;

    property Name: string read FName write SetName;
    property CorrectedName: string read GetCorrectedName;
    property Items: TList<TStubField> read FItems write FItems;
    property PureClassName: string read FPureClassName write SetPureClassName;
  end;

  TPkgJsonMapper = class
  private
    FTreeView: TTreeView;
    FClasses: TList<TStubClass>;
    FRootClass: TStubClass;
    FUnitName: string;
    procedure SetUnitName(const Value: string);
  protected
    function GetJsonType(AJsonValue: TJsonValue; CurrentType: TJsonType = jtUnknown): TJsonType;
    function GetJsonNumericType(AJsonValue: TJsonValue; CurrentType: TJsonType = jtUnknown): TJsonType;
    function GetFirstArrayItem(AJsonValue: TJsonValue): TJsonValue;
    procedure ProcessJsonObject(AJsonValue: TJsonValue; AParentClass: TStubClass);
    procedure ClearClasses;
    procedure InternalFormatTreeViewFields(AItem: TTreeViewItem);
    procedure FormatFields(ATreeView: TTreeView);
    procedure InternalVisualize(ATreeViewItem: TTreeViewItem; AClass: TStubClass; AItemStyleLookup: string);
    function  SuggestClassName(ASuggestedClassName: string): string;
  public
    constructor Create(ATreeView: TTreeView);
    destructor  Destroy; override;
    //  Parses a JSON string and creates internal stub class structure
    procedure   Parse(AJsonString: string; ARootClassName: string = 'Root');
    //  Generates resultant unit
    function    GenerateUnit(JsonLibrary: TJsonLibrary): string;
    procedure   Debug(ALines: TStrings);
    //  Visualizes stub class structure in a treeview
    procedure   Visualize(ATreeView: TTreeView; AItemStyleLookup: string);

    property    DestinationUnitName: string read FUnitName write SetUnitName;
  end;

procedure PrettyPrintJSON(JSONValue: TJSONValue; OutputStrings: TStrings; indent: integer = 0);

var
  PointDsFormatSettings: TFormatSettings;

implementation

uses
  uUpdate;

var
  ReservedWords: TList<string>;

const
  INDENT_SIZE = 2;

//  http://stackoverflow.com/a/12198174
procedure PrettyPrintPair(JSONValue: TJSONPair; OutputStrings: TStrings; last: boolean; indent: integer);
const TEMPLATE = '%s:%s';
var
  line: string;
  newList: TStringList;
begin
  newList := TStringList.Create;
  try
    PrettyPrintJSON(JSONValue.JsonValue, newList, indent);
    line := Format(TEMPLATE, [JSONValue.JsonString.ToString, Trim(newList.Text)]);
  finally
    newList.Free;
  end;

  line := StringOfChar(' ', indent + INDENT_SIZE) + line;
  if not last then
    line := line + ',';
  OutputStrings.Add(line);
end;

procedure PrettyPrintArray(JSONValue: TJSONArray; OutputStrings: TStrings; last: boolean; indent: integer);
var i: integer;
begin
  OutputStrings.Add(StringOfChar(' ', indent + INDENT_SIZE) + '[');
  for i := 0 to JSONValue.Count - 1 do
  begin
    PrettyPrintJSON(JSONValue.Items[i], OutputStrings, indent);
    if i < JSONValue.Count - 1 then
      OutputStrings[OutputStrings.Count-1] := OutputStrings[OutputStrings.Count-1] + ',';
  end;
  OutputStrings.Add(StringOfChar(' ', indent + INDENT_SIZE - 2) + ']');
end;

procedure PrettyPrintJSON(JSONValue: TJSONValue; OutputStrings: TStrings; indent: integer = 0);
var
  i: integer;
  LIdent: integer;
begin
  LIdent := indent + INDENT_SIZE;
  i := 0;

  if JSONValue is TJSONObject then
  begin
    OutputStrings.Add(StringOfChar(' ', LIdent) + '{');
    for i := 0 to TJSONObject(JSONValue).Count - 1 do
      PrettyPrintPair(TJSONObject(JSONValue).Pairs[i], OutputStrings, i = TJSONObject(JSONValue).Count - 1, LIdent);
    OutputStrings.Add(StringOfChar(' ', LIdent) + '}');
  end
  else if JSONValue is TJSONArray then
    PrettyPrintArray(TJSONArray(JSONValue), OutputStrings, i = TJSONObject(JSONValue).Count - 1, LIdent)
  else OutputStrings.Add(StringOfChar(' ', LIdent) + JSONValue.ToJSON);
end;


{ TPkgJsonMapper }

procedure TPkgJsonMapper.ProcessJsonObject(AJsonValue: TJsonValue; AParentClass: TStubClass);
var
  LJsonObj: TJSONObject;
  LJsonPair: TJSONPair;
  LJsonVal,
  LJsonVal2: TJSONValue;
  LJsonType,
  LJsonType2: TJsonType;
  LClass: TStubClass;
  LField: TStubField;
  LJsonItem: TJSONValue;
begin
  LJsonObj := AJsonValue as TJSONObject;

  for LJsonPair in LJsonObj do
  begin
    LJsonVal := LJsonPair.JsonValue;
    LJsonType := GetJsonType(LJsonVal);

    case LJsonType of
      jtObject:
      begin
        LField := AParentClass.FindField(LJsonPair.JsonString.Value);
        if not Assigned(LField) then begin
          LClass := TStubClass.Create(AParentClass, LJsonPair.JsonString.Value, Self);
          TStubObjectField.Create(AParentClass, LJsonPair.JsonString.Value, LClass);
        end else begin
          if LField is TStubContainerField then
            LClass := TStubContainerField(LField).FieldClass;
        end;
        if Assigned(LClass) then
          ProcessJsonObject(LJsonVal, LClass);
      end;

      jtArray:
      begin
        LClass := nil;
        LJsonType2 := jtUnknown;

        LJsonVal2 := GetFirstArrayItem(LJsonVal);
        if LJsonVal2 <> nil then
        begin
          LJsonType2 := GetJsonType(LJsonVal2);
          case LJsonType2 of
            jtObject:
            begin // needs to have a find here instead of just Create!
              LField := AParentClass.FindField(LJsonPair.JsonString.Value);
              if not Assigned(LField) then begin
                LClass := TStubClass.Create(AParentClass, LJsonPair.JsonString.Value, Self);
              end  else begin
                //LField is TStubObjectField, but we need the TStubClass
                case LField.FieldType of
                  jtObject: LClass := (LField as TStubObjectField).FieldClass;
                  jtArray: LClass := (LField as TStubArrayField).FieldClass;
                  else raise EJsonMapper.Create('Elements must be of the same type!');
                end;
              end;
              for LJsonItem in (LJsonVal as TJsonArray) do
                ProcessJsonObject(LJsonItem, LClass);
            end;
            jtArray: raise EJsonMapper.Create('Nested Arrays are not supported!');
          end;
        end;

        if LJsonType2 <> jtUnknown then begin
          LField := AParentClass.FindField(LJsonPair.JsonString.Value);
          if not Assigned(LField) then
            TStubArrayField.Create(AParentClass, LJsonPair.JsonString.Value, LJsonType2, LClass);
        end;
      end;
      jtInteger, jtInt64, jtUnixTimestamp, jtExtended,
      jtString,
      jtDate,
      jtDateTime,
      jtTrue,
      jtFalse: begin
        LField := AParentClass.FindField(LJsonPair.JsonString.Value);
        if not Assigned(LField) then
          TStubField.Create(AParentClass, LJsonPair.JsonString.Value, LJsonType);
      end;
    end;
  end;

  AParentClass.SortFields;
end;


function TPkgJsonMapper.GenerateUnit(JsonLibrary: TJsonLibrary): string;
var
  LClass: TStubClass;
  k: integer;
  LList: TStringList;
  libraryUses: string;
begin
  LList := TStringList.Create;
  try
    case JsonLibrary of
      jlDelphi: libraryUses := 'Rest.JSON';
      jlGrijjy: libraryUses := 'Grijjy.Bson.Serialization';
      jlXSuper: libraryUses := 'XSuperObject';
    end;
    LList.Add('unit ' + FUnitName + ';');
    LList.Add('');
    LList.Add('interface');
    LList.Add('');
    LList.Add('uses'+sLineBreak+
      '  Generics.Collections, '+ libraryUses +';');
    LList.Add('');
    LList.Add('type');

    for k := FClasses.Count - 1 downto 0 do
    begin
      LClass := FClasses[k];
      LList.Add(LClass.GetDeclarationPart(k = 0, JsonLibrary).TrimRight);
      LList.Add('');
    end;

    LList.Add('implementation');

    for k := FClasses.Count - 1 downto 0 do
    begin
      LClass := FClasses[k];
      LList.Add(LClass.GetImplementationPart(JsonLibrary, k = 0).TrimRight);
    end;

    LList.Add('');
    LList.Add('end.');

    Result := LList.Text;

  finally
    LList.Free;
  end;

end;

procedure TPkgJsonMapper.Visualize(ATreeView: TTreeView; AItemStyleLookup: string);
var
  LItem: TTreeViewItem;
begin
  ATreeView.Clear;
  if FRootClass <> nil then
  begin
    ATreeView.BeginUpdate;
    LItem := TTreeViewItem.Create(ATreeView);
    LItem.Text := FRootClass.Name;
    LItem.TagObject := FRootClass;
    LItem.WordWrap := false;
    ATreeView.AddObject(LItem);
    InternalVisualize(LItem, FRootClass, AItemStyleLookup);
    FormatFields(ATreeView);
    ATreeView.EndUpdate;
    ATreeView.ExpandAll;
  end;
end;

function TPkgJsonMapper.GetFirstArrayItem(AJsonValue: TJsonValue): TJsonValue;
var
  LJsonArray: TJsonArray;
  LJsonValue: TJSONValue;
begin
  Result := nil;
  LJsonArray := AJsonValue as TJsonArray;
  for LJsonValue in LJsonArray do
  begin
    Result := LJsonValue;
    break;
  end;
end;

procedure TPkgJsonMapper.ClearClasses;
var
  LClass: TStubClass;
begin
  for LClass in FClasses do
  begin
    LClass.Free;
  end;

  FClasses.Clear;
end;

constructor TPkgJsonMapper.Create(ATreeView: TTreeView);
begin
  inherited Create;
  FTreeView := ATreeView;
  FClasses := TList<TStubClass>.Create;
end;

procedure TPkgJsonMapper.Debug(ALines: TStrings);
var
  LClass: TStubClass;
  LField: TStubField;
begin
  ALines.Clear;

  for LClass in FClasses do
  begin
    ALines.Add('-------');
    ALines.Add(LClass.Name);
    for LField in LClass.FItems do
    begin
      ALines.Add(Format('%-15s | %s', [LField.FieldName, LField.GetTypeAsString]));
    end;
  end;
end;

destructor TPkgJsonMapper.Destroy;
begin
  ClearClasses;
  FreeAndNil(FClasses);
  inherited;
end;

procedure TPkgJsonMapper.FormatFields(ATreeView: TTreeView);
begin
  if ATreeView.Count = 1 then
  begin
    InternalFormatTreeViewFields(ATreeView.Items[0]);
  end;
end;

procedure TPkgJsonMapper.SetUnitName(const Value: string);
begin
  FUnitName := Value;
end;

function TPkgJsonMapper.SuggestClassName(ASuggestedClassName: string): string;
var
  LClass: TStubClass;
  LMax, LVal: integer;
  LString: string;
begin
  Result := ASuggestedClassName;
  LMax := 0;
  for LClass in FClasses do
  begin
    if LClass.Name.StartsWith(ASuggestedClassName, true) then
    begin
      LString := Copy(LClass.Name, length(ASuggestedClassName) + 2);
      if (LString.Length = 3) then
      begin
        if TryStrToInt(LString, LVal) then
        begin
          inc(LVal);
          if LVal > LMax then
            LMax := LVal;
        end;
      end
      else
        LMax := 1;
    end;
  end;

  if LMax > 0 then
    Result := Format('%s_%0.3d', [ASuggestedClassName, LMax]);
end;

function TPkgJsonMapper.GetJsonType(AJsonValue: TJsonValue; CurrentType: TJsonType = jtUnknown): TJsonType;
var
  LJsonString: TJSONString;
begin
  if AJsonValue is TJSONObject then
    Result := jtObject
  else if AJsonValue is TJSONArray then
    Result := jtArray
  else if (AJsonValue is TJSONNumber) then
    Result := GetJsonNumericType(AJsonValue, CurrentType) // jtNumber
  else if AJsonValue is TJSONTrue then
    Result := jtTrue
  else if  AJsonValue is TJSONFalse then
    Result := jtFalse
  else if AJsonValue is TJSONString then begin
    LJsonString := (AJsonValue as TJSONString);
    if TRegEx.IsMatch(LJsonString.Value, '^([0-9]{4})-?(1[0-2]|0[1-9])-?(3[01]|0[1-9]|[12][0-9])(T| )' +
      '(2[0-3]|[01][0-9]):?([0-5][0-9]):?([0-5][0-9])(Z|-[0-2][0-9]:[0-5][0-9])$') then
      Result := jtDateTime
    else if TRegEx.IsMatch(LJsonString.Value, '^([0-9]{4})(-?)(1[0-2]|0[1-9])\2(3[01]|0[1-9]|[12][0-9])$') then
      Result := jtDate
    else
      Result := jtString
  end else
    Result := jtUnknown;
end;

function TPkgJsonMapper.GetJsonNumericType(AJsonValue: TJsonValue; CurrentType: TJsonType = jtUnknown): TJsonType;

  function IsFloat(Source: string): Integer;
  var
    TReal: Double;
    E: Integer;
  begin
    Source := Trim(Source);
    Val(Source, TReal, E);
    if E = 0 then
      Result := 1
    else
      Result := 0;

    if (Result = 1) and (Pos('.', Source) = 0) then
      Result := 2; //definite maybe

    if (Result = 2) and (TReal > 2147483640.0) then
      Result := 0;
  end;

  function IsInteger(Source: string): Boolean;
  var
    TInt: Integer;
    E: Integer;
  begin
    Source := Trim(Source);
    Result := Pos('.', Source) = 0;
    if Result then begin
      Val(Source, TInt, E);
      if TInt=1 then begin end;//Removes warning.
      Result := (E = 0);
    end;
  end;

  function IsInt64(Source: string): Boolean;
  var
    Value: Int64;
  begin
    Value := StrToInt64Def(Source, Low(Int64));
    Result := (Value > Low(Int64)) and (Value > High(Integer));
  end;

  function IsUnixTimestamp(Source: string): Boolean;
  //  473385600  473385600000 = January 1, 1985 12:00:00 AM
  // 2556141803 2556141803000 = December 31, 2050 11:23:23 PM
  var
    Value: Int64;
  begin
    Value := StrToInt64Def(Source, Low(Int64));
    Result := ((Value > 473385600000) and (Value < 2556141803000));
  end;

var
  intTemp: Integer;
  fieldData: string;
begin
  Result := CurrentType;
  if Result = jtExtended then
    Exit;

  // detect type from simplest to most complex = jtInteger, jtInt64, jtUnixTimestamp, jtExtended

  fieldData := AJsonValue.ToString;

  if (Result in [jtUnknown, jtInteger]) and (Result <> jtExtended) then
  try
    if IsInt64(fieldData) then
      Result  := jtInt64
    else
    if IsInteger(fieldData) then
      Result  := jtInteger;
  except
    Result  := jtUnknown;
  end;

  if Result in [jtUnknown, jtInteger, jtExtended] then
  try
    case IsFloat(fieldData) of
      0: Result := jtUnknown; //definetly not a float
      1: if Result in [jtUnknown, jtInteger] then
        Result := jtExtended;
      2: if Result = jtUnknown then
        Result := jtInteger;
    end;
  except
    if Result = jtExtended then
      Result  := jtUnknown;
  end;

  if (Result = jtInt64) and IsUnixTimestamp(fieldData) then
    Result := jtUnixTimestamp;

  if Result = jtUnknown then
    Result := jtExtended;
  // jtInteger, jtInt64, jtExtended, jtUnixTimestamp
end;

procedure TPkgJsonMapper.InternalFormatTreeViewFields(AItem: TTreeViewItem);
var
  LItem: TTreeViewItem;
  k: Integer;
  LSize, LPos: integer;
begin
  LSize := 0;

  //  Find max len
  for k := 0 to AItem.Count - 1 do
  begin
    LItem := AItem.Items[k];
    LPos := Pos(':', LItem.Text);
    if (LPos > 0) AND (LPos > LSize) then
      LSize := LPos;
  end;

  for k := 0 to AItem.Count - 1 do
  begin
    LItem := AItem.Items[k];
    LPos := LSize - Pos(':', LItem.Text);
    if (LPos > 0) then
      LItem.Text := LItem.Text.Replace(':', StringOfChar(' ', LPos) + ':');

    InternalFormatTreeViewFields(LItem);
  end;

end;

procedure TPkgJsonMapper.InternalVisualize(ATreeViewItem: TTreeViewItem;
  AClass: TStubClass; AItemStyleLookup: string);
var
  LField: TStubField;
  LItem: TTreeViewItem;
begin
  for LField in AClass.FItems do
  begin

    LItem := TTreeViewItem.Create(ATreeViewItem);
    LItem.StyleLookup := AItemStyleLookup;
    LItem.TagObject := LField;
    LItem.WordWrap := false;

    case LField.FieldType of
      jtObject:
      begin
        LItem.Text := LField.CorrectedName + ': {} ' + LField.GetTypeAsString;
        InternalVisualize(LItem, (LField as TStubObjectField).FieldClass, AItemStyleLookup);
      end;

      jtArray:
      begin
        LItem.Text := LField.CorrectedName + ': [] ' + LField.GetTypeAsString;
        if (LField as TStubArrayField).ContainedType = jtObject then
        begin
          InternalVisualize(LItem, (LField as TStubArrayField).FieldClass, AItemStyleLookup);
        end;
      end;

      else
      begin
        LItem.Text := LField.CorrectedName + ': ' + LField.GetTypeAsString;
      end;
    end;

    ATreeViewItem.AddObject(LItem);

  end;
end;

procedure TPkgJsonMapper.Parse(AJsonString: string; ARootClassName: string);
var
  LJsonValue,
  LJsonValue2, LJsonItem: TJSONValue;
  LJsonType: TJsonType;
  LClass: TStubClass;
begin
  ClearClasses;

  LJsonValue := TJSONObject.ParseJSONValue(AJsonString);
  if LJsonValue <> nil then
  begin
    try
      FRootClass := TStubClass.Create(nil, ARootClassName, self);

      case GetJsonType(LJsonValue) of
        jtObject:
        begin
          ProcessJsonObject(LJsonValue, FRootClass);
        end;

        jtArray:
        begin
          LClass := nil;
          LJsonType := jtUnknown;

          LJsonValue2 := GetFirstArrayItem(LJsonValue);
          if LJsonValue2 <> nil then
          begin
            LJsonType := GetJsonType(LJsonValue2);
            LClass := TStubClass.Create(FRootClass, 'Item', Self);
          end;
          if LJsonType <> jtUnknown then
          begin
            TStubArrayField.Create(FRootClass, 'Items', LJsonType, LClass);
            if LJsonType = jtObject then
            begin
              for LJsonItem in TJsonArray(LJsonValue) do
                ProcessJsonObject(LJsonItem, LClass);
            end else
              ProcessJsonObject(LJsonValue2, LClass);
          end;
        end;
      end;
    finally
      LJsonValue.Free;
    end;
  end
  else
    raise EJsonMapper.Create('Unable to parse the JSON String!');

  FTreeView.ExpandAll;
end;

{ TVirtualClass }

constructor TStubClass.Create(AParentClass: TStubClass; AClassName: string; AMapper: TPkgJsonMapper);
begin
  inherited Create;
  FMapper := AMapper;
  Name := AClassName;

  FItems := TList<TStubField>.Create;
  FComplexItems := TList<TStubField>.Create;
  FArrayItems := TList<TStubField>.Create;
  FMapper.FClasses.Add(self);

  FParentClass := AParentClass;

  FComparison :=
    function(const Left, Right: TStubField): Integer
    begin
      if Left.FName > Right.FName then
        Result := 1
      else
        if Left.FName < Right.FName then
          Result := -1
        else
          Result := 0;
    end;

  FComparer := TComparer<TStubField>.Construct(FComparison);

end;

destructor TStubClass.Destroy;
var
  LItem: TStubField;
begin

  //  ToArray is needed because stub field remove themselves from FItems
  for LItem in FItems.ToArray do
  begin
    LItem.Free;
  end;

  FreeAndNil(FComplexItems);
  FreeAndNil(FItems);
  FreeAndNil(FArrayItems);
  inherited;
end;

function TStubClass.FindField(AFieldName: string): TStubField;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Items.Count - 1 do
    if SameText(Items[i].Name, AFieldName) then begin
      Result := Items[i];
      Break;
    end;
end;

function TStubClass.GetImplementationPart(AJsonLibrary: TJsonLibrary; IncludeSerializers: Boolean = False): string;
var
  LLines: TStringList;
  LString: string;
  LClassName: string;
  LItem: TStubField;
  toJsonString: string;
  fromJsonString: string;
begin
  LLines := TStringList.Create;
  try
    LClassName := Format('%s', [FName]);
    if IncludeSerializers or (FComplexItems.Count + FArrayItems.Count > 0) then
    begin
      LLines.Add('');
      LLines.Add(Format('{ %s }', [LClassName]));
      LLines.Add('');
    end;
    if FComplexItems.Count > 0 then
    begin
      LLines.Add(Format('constructor %s.Create;', [LClassName]));
      LLines.Add('begin');
      LLines.Add('  inherited;');

      for LItem in FComplexItems do
      begin
        LString := Format('  %s := %s.Create;', [LItem.FieldName, (LItem).GetTypeAsString]);
        LLines.Add(LString);
      end;

      LLines.Add('end;');
      LLines.Add('');
    end;

    if (FComplexItems.Count > 0) OR (FArrayItems.Count > 0) then
    begin
      LLines.Add(Format('destructor %s.Destroy;', [LClassName]));

      if FArrayItems.Count > 0 then
      begin
        LLines.Add('var');
        for LItem in FArrayItems do
        begin
          LString := Format('  %sItem: %s;', [LItem.FName, (LItem as TStubContainerField).FFieldClass.CorrectedName]);
          LLines.Add(LString);
        end;
      end;

      LLines.Add('begin');

      if FArrayItems.Count > 0 then
      begin
      //LLines.Add('');
        for LItem in FArrayItems do
        begin
          LLines.Add(Format('  for %sItem in %s do', [LItem.FName, LItem.FieldName]));
          LLines.Add(Format('    %sItem.Free;', [LItem.FName]));
        end;
        LLines.Add('');
      end;

      for LItem in FComplexItems do
      begin
        LString := Format('  %s.Free;', [LItem.FieldName]);
        LLines.Add(LString);
      end;

      LLines.Add('  inherited;');
      LLines.Add('end;')
    end;

    if includeSerializers then
    begin
      case AJsonLibrary of
        jlDelphi: begin
          toJsonString   := '  Result := TJson.ObjectToJsonString(self);';
          fromJsonString := Format('  Result := TJson.JsonToObject<%s>(AJsonString)', [LClassName]);
        end;
        jlGrijjy: begin
          toJsonString   := '  TgoBsonSerializer.Serialize(Self, Result);';
          fromJsonString := '  Result := nil;'#13#10 +
                            '  TgoBsonSerializer.Deserialize(AJsonString, Result);';
        end;
        jlXSuper: begin
          toJsonString   := '  Result := Self.AsJSON;';
          fromJsonString := '  Result := Self.FromJSON(AJsonString);';
        end;
      end;
      LLines.Add('');
      LLines.Add(Format('function %s.ToJsonString: string;', [LClassName]));
      LLines.Add('begin');
      LLines.Add(toJsonString);
      LLines.Add('end;');

      LLines.Add('');
      LLines.Add(Format('class function %s.FromJsonString(const AJsonString: string): %s;', [LClassName, LClassName]));
      LLines.Add('begin');
      LLines.Add(fromJsonString);
      LLines.Add('end;');
    end;

    Result := LLines.Text;
  finally
    LLines.Free;
  end;
end;

procedure TStubClass.SetName(const Value: string);
var
  LName: string;
begin
  FPureClassName := Value;
  if FPureClassName.Chars[0] = '@' then
    Delete(FPureClassName, 1, 1); // handle @context item

  FPureClassName := string(UpCase(FPureClassName.Chars[0])) + FPureClassName.Substring(1);

  if FPureClassName.EndsWith('s') then // remove plural element
    SetLength(FPureClassName, FPureClassName.Length - 1);

  LName := 'T' + FPureClassName {+ 'Class'};

  FName := FMapper.SuggestClassName(LName);
end;

procedure TStubClass.SetPureClassName(const Value: string);
begin
  FPureClassName := Value;
end;

function TStubClass.GetCorrectedName: string;
begin
  Result := Name;
  if Result.Chars[0] = '@' then
    Delete(Result, 1, 1); // handle @context item
end;

procedure TStubClass.SortFields;
begin
  // FItems.Sort(FComparer);
end;

function TStubClass.GetDeclarationPart(IncludeSerializers: Boolean; JsonLibrary: TJsonLibrary): string;
var
  LLines: TStringList;
  LString: string;
  LItem: TStubField;
begin
  LLines := TStringList.Create;
  try
    LLines.Add('  ' + FName + ' = class');
    LLines.Add('  private');

    for LItem in FItems do
    begin
      LString := Format('  %s: %s;', [LItem.FieldName, LItem.GetTypeAsString]);
      LLines.Add('  ' + LString);
    end;

    LLines.Add('  public');

    if FComplexItems.Count > 0 then
    begin
      LLines.Add('    constructor Create;');
    end;

    if (FComplexItems.Count > 0) OR (FArrayItems.Count > 0) then
    begin
      LLines.Add('    destructor Destroy; override;');
    end;

    if includeSerializers then
    begin
      LLines.Add('    function ToJsonString: string;');
      LLines.Add(Format('    class function FromJsonString(const AJsonString: string): %s;', [FName]));
    end;

    if IncludeSerializers or (FComplexItems.Count + FArrayItems.Count > 0) then
      LLines.Add('');

    for LItem in FItems do
    begin
      if (LItem.FieldType = jtUnknown) OR ((LItem is TStubContainerField) AND ((LItem as TStubContainerField).ContainedType = jtUnknown)) then
        raise EJsonMapper.CreateFmt('The property [%s] has unknown type!', [LItem.PropertyName]);

      if LItem.AddNameAttrib then begin
        case JsonLibrary of
          jlDelphi: LString := '[JsonName(''%s'')]';
          jlGrijjy: LString := '[BsonElement(''%s'')]';
          jlXSuper: LString := '[alias(''%s'')]';
        end;
        LLines.Add('    ' + Format(LString, [LItem.Name]));
      end;

      LString := Format('  property %s: %s read %s write %s;', [LItem.PropertyName, LItem.GetTypeAsString, LItem.FieldName, LItem.FieldName]);
      LLines.Add('  ' + LString);
    end;

    LLines.Add('  end;');

    Result := LLines.Text;
  finally
    LLines.Free;
  end;
end;

{ TVirtualClassItemBase }

constructor TStubField.Create(AParentClass: TStubClass; AItemName: string; AFieldType: TJsonType);
begin
  inherited Create;

  if AItemName.Contains('-') then
    raise EJsonMapper.CreateFmt('%s: Hyphens are not allowed!', [AItemName]);

  FFieldType := AFieldType;
  Name := AItemName;

  FParentClass := AParentClass;
  if FParentClass <> nil then
    FParentClass.FItems.Add(self);
end;

destructor TStubField.Destroy;
begin
  if FParentClass <> nil then
    FParentClass.FItems.Remove(self);
  inherited;
end;

class function TStubField.GetTypeAsString(AType: TJsonType): string;
begin
  case AType of
    jtUnknown: Result := 'Unknown';
    jtString: Result := 'string';
    jtTrue,
    jtFalse: Result := 'Boolean';
    jtInteger: Result := 'Integer';
    jtInt64: Result := 'Int64';
    jtUnixTimestamp: Result := 'TDateTime'; //TODO: How to switch based on Library
    jtExtended: Result := 'Extended';
    jtDate: Result := 'TDate';
    jtDateTime: Result := 'TDateTime';
    jtBytes: Result := 'Byte';
  end;
end;

procedure TStubField.SetName(const Value: string);
begin
  FName := Value;

  FFieldName := Value;
  if FFieldName.Chars[0] = '@' then
    Delete(FFieldName, 1, 1); // handle @context fields

  if ReservedWords.Contains(FFieldName.ToLower) then
    FPropertyName := '&' + FFieldName
  else
    FPropertyName := FFieldName;

  FFieldName := 'F' + string(UpCase(FFieldName.Chars[0])) + FFieldName.Substring(1); // Copy(Value, 2);
end;

function TStubField.GetTypeAsString: string;
begin
  Result := GetTypeAsString(FFieldType);
end;

function TStubField.GetCorrectedName: string;
begin
  Result := Name;
  if Result.Chars[0] = '@' then
    Delete(Result, 1, 1); // handle @context item
end;

function TStubField.GetAddNameAttrib: boolean;
begin
  Result := Name <> CorrectedName;
end;

{ TArrayItem }

constructor TStubArrayField.Create(AClass: TStubClass; AItemName: string; AItemSubType: TJsonType; AItemClass: TStubClass);
begin
  inherited Create(AClass, AItemName, jtArray);
  FContainedType := AItemSubType;
  FFieldClass := AItemClass;
  if FContainedType = TJsonType.jtObject then
    AClass.FArrayItems.Add(self);
end;

function TStubArrayField.GetTypeAsString: string;
var
  LSubType: string;
begin
  case FContainedType of
    jtObject: LSubType := FFieldClass.Name;
    jtArray: raise EJsonMapper.Create('Nested arrays are not supported!');
    else
      LSubType := GetTypeAsString(FContainedType);
  end;
  Result := Format('TArray<%s>', [LSubType]);
end;

{ TStubObjectField }

constructor TStubObjectField.Create(AParentClass: TStubClass; AItemName: string; AItemClass: TStubClass);
begin
  inherited Create(AParentClass, AItemName, jtObject);
  FFieldClass := AItemClass;
  AParentClass.FComplexItems.Add(self);
  FContainedType := jtObject;
end;

function TStubObjectField.GetTypeAsString: string;
begin
  Result := FFieldClass.Name;
end;

initialization

  PointDsFormatSettings := TFormatSettings.Create();
  PointDsFormatSettings.DecimalSeparator := '.';

  ReservedWords := TList<string>.Create;
  ReservedWords.Add('type');
  ReservedWords.Add('for');
  ReservedWords.Add('var');
  ReservedWords.Add('begin');
  ReservedWords.Add('end');
  ReservedWords.Add('function');
  ReservedWords.Add('procedure');
  ReservedWords.Add('class');
  ReservedWords.Add('record');
  ReservedWords.Add('string');
  ReservedWords.Add('label');
  ReservedWords.Add('initialization');
  ReservedWords.Add('finalization');


finalization

  FreeAndNil(ReservedWords);

end.
