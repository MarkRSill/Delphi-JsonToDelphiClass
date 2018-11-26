unit uUpdate;

interface

uses
  REST.Client, uGitHub, REST.JSON, JSON,
  IPPeerClient, SysUtils, System.Threading, Classes, Pkg.Json.Mapper;

const
  ProgramVersion : double = 0.65;
  UpdateUrl = 'https://api.github.com/repos/PKGeorgiev/Delphi-JsonToDelphiClass/releases';
  ProgramUrl = 'https://github.com/PKGeorgiev/Delphi-JsonToDelphiClass';

function InternalCheckForUpdate: TObject;
procedure NewCheckForUpdateTask(AOnFinish: TProc<TObject>);

implementation

uses
  Math;

function InternalCheckForUpdate: TObject;
var
  LRestClient: TRESTClient;
  LRestRequest: TRESTRequest;
  LRestResponse: TRESTResponse;
  LRelease: TObject;
  LJsonArray: TJsonArray;
  LJsonValue: TJsonValue;
  LTag: double;
begin
  Result := nil;
  try
    LRestClient := TRESTClient.Create('');
    try
      LRestClient.BaseURL := UpdateUrl;
      LRestResponse := TRESTResponse.Create(nil);
      try
        LRestRequest := TRESTRequest.Create(nil);
        try
          LRestRequest.Client := LRestClient;
          LRestRequest.Response := LRestResponse;
          LRestRequest.Timeout := 10000;

          LRestRequest.Execute;

          if LRestResponse.StatusCode = 200 then begin
            LJsonArray := TJSONObject.ParseJSONValue(LRestResponse.Content) as TJSONArray;
            try
              for LJsonValue in LJsonArray do begin
                LRelease := TReleaseClass.FromJsonString(LJsonValue.ToJSON);
                LTag := StrToFloat((LRelease as TReleaseClass).tag_name, PointDsFormatSettings);
                if Math.CompareValue(LTag, ProgramVersion) = 1 then begin
                  Result := LRelease;
                  break;
                end else
                  LRelease.Free;
              end;
            finally
              LJsonArray.Free;
            end;
          end else
            Result := TErrorClass.FromJsonString(LRestResponse.Content);
        finally
          LRestRequest.Free;
        end;
      finally
        LRestResponse.Free;
      end;
    finally
      LRestClient.Free;
    end;
  except
    on e: Exception do begin
      Result := TErrorClass.Create;
      (Result as TErrorClass).message := e.Message;
    end;
  end;
end;

procedure NewCheckForUpdateTask(AOnFinish: TProc<TObject>);
begin
  TTask.Run(
    procedure
    var
      LResult: TObject;
    begin
      //  Asynchronously check for update
      LResult := InternalCheckForUpdate();
      try
        // Execute AOnFinish in the context of the Main Thread
        TThread.Synchronize(nil,
          procedure
          begin
            AOnFinish(LResult);
          end
        );
      except
      end;
    end
  );
end;

end.
