unit svDB;

interface
uses
  Classes,
  DB;

type

  { TsvWideStringField }

  TsvWideStringField = class(TWideStringField)
  protected
    function GetDataSize: Integer; override;
  end;


  { TsvWideMemoField }

  TsvWideMemoField = class(TMemoField)
  private
    function GetAsWideString: WideString;
    procedure SetAsWideString(const Value: WideString);
  protected
    function GetClassDesc: string; override;
    function GetAsVariant: Variant; override;
    procedure SetVarValue(const Value: Variant); override;
  public
    property Value: WideString read GetAsWideString write SetAsWideString;
  end;


implementation
uses
  SysUtils;


{ TsvWideStringField }

function TsvWideStringField.GetDataSize: Integer;
begin
  Result := Size + SizeOf(WideChar);    // + null termination
end;

{ TsvWideMemoField }

function TsvWideMemoField.GetAsWideString: WideString;
var
  Len: Integer;
begin
  with DataSet.CreateBlobStream(Self, bmRead) do
  try
    Len := Size;
    SetString(Result, nil, Len div SizeOf(WideChar));
    ReadBuffer(PWideChar(Result)^, Len);
  finally
    Free;
  end;
end;

procedure TsvWideMemoField.SetAsWideString(const Value: WideString);
begin
  with DataSet.CreateBlobStream(Self, bmWrite) do
  try
    WriteBuffer(PWideChar(Value)^, Length(Value) * SizeOf(WideChar));
  finally
    Free;
  end;
end;

function TsvWideMemoField.GetClassDesc: string;
begin
  Result := '(WideMemo)';
  if not IsNull then Result := AnsiUpperCase(Result);
end;

function TsvWideMemoField.GetAsVariant: Variant;
begin
  Result := GetAsWideString;
end;

procedure TsvWideMemoField.SetVarValue(const Value: Variant);
begin
  SetAsWideString(Value);
end;

initialization
  RegisterClass(TsvWideStringField);
  RegisterClass(TsvWideMemoField);

end.

