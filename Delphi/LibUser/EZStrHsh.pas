unit EZStrHsh;
  {-Example unit defining a hash table for strings}

{$I EzdslDef.inc}
{---Place any compiler options you require here----------------------}


{--------------------------------------------------------------------}
{$I EzdslOpt.inc}

interface

uses
  SysUtils,
  EzdslSup,
  EzdslHsh;

type
  {A hash table for storing strings}
  TStringHashTable = class
    private
      HashTable : THashTable;

      function GetTableSize: Integer;
      procedure SetTableSize(Value: Integer);

      function GetIgnoreCase: Boolean;
      procedure SetIgnoreCase(Value: Boolean);

      function GetHashFunction: THashFunction;
      procedure SetHashFunction(Value: THashFunction);

    public
      constructor Create;
        {-Initialise the hash table}
      destructor Destroy; override;
        {-Destroy the hash table}

      function Count : longint;
        {-Return the number of strings in the hash table}
      function IsEmpty : boolean;
        {-Return true if the hash table is empty}
      procedure Erase(const aKey : string);

      function Examine(const aKey : string) : String;
      procedure Insert(const aKey : string; aData : String);
      function Search(const aKey : string; var aData : String): boolean;
      function Iterate2(Action: THashTableIterator; Backwards: Boolean;
          ExtraData: Pointer): Pointer;

      property TableSize: Integer read GetTableSize write SetTableSize;
      property IgnoreCase: Boolean read GetIgnoreCase write SetIgnoreCase;
      property HashFunction: THashFunction read GetHashFunction
          write SetHashFunction;
  end;

implementation


{===TStringStack implementation======================================}
constructor TStringHashTable.Create;
begin
  HashTable := THashTable.Create(true);
  HashTable.DisposeData := EZStrDisposeData;
end;
{--------}
destructor TStringHashTable.Destroy;
begin
  HashTable.Free;
end;
{--------}
function TStringHashTable.Count : longint;
begin
  Count := HashTable.Count;
end;
{--------}
function TStringHashTable.IsEmpty : boolean;
begin
  IsEmpty := HashTable.IsEmpty;
end;
{--------}
procedure TStringHashTable.Erase(const aKey : string);
begin
  HashTable.Erase(aKey);
end;

function TStringHashTable.Examine(const aKey : string) : String;
var
  PS : PEZString;
begin
  Result := '';
  if not IsEmpty then begin
    PS := PEZString(HashTable.Examine(aKey));
    if Assigned(PS) then begin
      Result := PS^;
    end;
  end;
end;

procedure TStringHashTable.Insert(const aKey : string; aData : String);
var
  PS : PEZString;
begin
  PS := EZStrNew(aData);
  try
    HashTable.Insert(aKey, PS);
  except
    on EEZContainerError do begin
      HashTable.DisposeData(PS);
      raise;
    end;
  end; {try..except}
end;

function TStringHashTable.Search(const aKey : string;
  var aData : String): boolean;
var
  PS : PEZString;
begin
  aData := '';
  Result := HashTable.Search(aKey, Pointer(PS));
  if Result then
    aData := PS^;
end;

function TStringHashTable.Iterate2(Action: THashTableIterator; Backwards: Boolean;
  ExtraData: Pointer): Pointer;
begin
  Result := HashTable.Iterate2(Action, Backwards, ExtraData);
end;

function TStringHashTable.GetTableSize: Integer;
begin
  Result := HashTable.TableSize;
end;

procedure TStringHashTable.SetTableSize(Value: Integer);
begin
  HashTable.TableSize := Value;
end;

function TStringHashTable.GetIgnoreCase: Boolean;
begin
  Result := HashTable.IgnoreCase;
end;

procedure TStringHashTable.SetIgnoreCase(Value: Boolean);
begin
  HashTable.IgnoreCase := Value;
end;

function TStringHashTable.GetHashFunction: THashFunction;
begin
  Result := HashTable.HashFunction;
end;

procedure TStringHashTable.SetHashFunction(Value: THashFunction);
begin
  HashTable.HashFunction := Value;
end;

end.