unit uMtrWriter;

interface

uses
  System.SysUtils, System.Classes,
  JS, Web, // Blob & DOM
  uTransFixed, uTransRecords,
  uMtrBuilderFactory, uPrestatieModel, uMtrBuilderIntf;

type
  TMtrWriter = class
  strict private
    FLines: TStringList;
    FHeader: TMtrHeader;
    FTrail: TMtrTrailer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ExportPrestatie(const P: TPrestatie);
    procedure SaveToFile(const FileName: string);
  end;

function BuildMtrFileName(const SenderCode: string; Seq: Integer): string;

implementation

{ --- ctor / dtor ------------------------------------------------------------ }

constructor TMtrWriter.Create;
begin
  FLines := TStringList.Create;
  FLines.TrailingLineBreak := False; // we voegen CRLF zelf toe
  FHeader.FileDate := Date;
  FHeader.Version := TRANS_VERSION;
  FHeader.ModuleVer := TRANS_MODULE_VER;
end;

destructor TMtrWriter.Destroy;
begin
  FLines.Free;
  inherited;
end;

{ --- export ---------------------------------------------------------------- }

procedure TMtrWriter.ExportPrestatie(const P: TPrestatie);
var
  Builder: IMtrBuilder;
begin
  Builder := GetBuilder(P.PrestType);
  FLines.Add(Builder.Build(P).ToLine.TrimRight); // TStringList voegt geen CRLF!
  Inc(FTrail.RecordCount);
end;

{ --- save ------------------------------------------------------------------ }

procedure TMtrWriter.SaveToFile(const FileName: string);
var
  L: TStringList;
  RawText: string;
  Blob: TJSBlob;
  Url: string;
begin
  L := TStringList.Create;
  try
    L.Text := FHeader.ToLine + FLines.Text + FTrail.ToLine;
    RawText := L.Text; // één string voor beide targets
  finally
    L.Free;
  end;

  { --------- WEB: Blob + auto-download ------------------------------------- }
  Blob := TJSBlob.new([RawText], TJSBlobPropertyBag.Create('text/plain;charset=windows-1252'));
  Url := TJSURL.createObjectURL(Blob);

  asm
    var a = document.createElement('a');
    a.href = Url;
    a.download = FileName;
    a.style.display = 'none';
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    window.URL.revokeObjectURL(Url);
  end;
end;

{ --- hulpfunctie ----------------------------------------------------------- }

function BuildMtrFileName(const SenderCode: string; Seq: Integer): string;
begin
  Result := UpperCase(SenderCode).PadRight(6).Substring(0, 6) + Seq.ToString.PadLeft(4, '0') + '.MTR';
end;

end.
