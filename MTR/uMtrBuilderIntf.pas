unit uMtrBuilderIntf;

interface

uses uPrestatieModel, uTransRecords;

type
  IMtrBuilder = interface
    ['{4F214B76-99C2-4E75-ADD9-51DC0AF1EE09}']
    function Build(const P: TPrestatie): TMtrDetail;
  end;

implementation

end.
