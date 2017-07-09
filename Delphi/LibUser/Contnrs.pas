{*******************************************************}
{                                                       }
{ DWPL Delphi Cross Platform Core Library (rtl)         }
{ Contnrs.pas                                           }
{                                                       }
{ for Delphi 4, 5, 6                                    }
{ Version 2.0                                           }
{ Copyright (c) 2001 by Immo Wache                      }
{ e-mail: immo.wache@dwp42.org                          }
{ www: http://www.dwp42.org                             }
{                                                       }
{ This file may be distributed and/or modified under    }
{ the terms of the GNU General Public License (GPL)     }
{ version 2 as published by the Free Software           }
{ Foundation and appearing at                           }
{ http://www.dwp42.org/license/gpl.html.                }
{                                                       }
{ this software contains portions from:                 }
{                                                       }
{   - Kylix and Delphi Cross-Platform Runtime Library   }
{     Copyright (C) 1996, 2001                          }
{     Borland Software Corporation                      }
{     www: http://www.borland.com                       }
{                                                       }
{ Please inform you about special copyrights for these  }
{ portions on the apropriate web pages.                 }
{                                                       }
{*******************************************************}

{ *************************************************************************** }
{                                                                             }
{ Kylix and Delphi Cross-Platform Runtime Library                             }
{                                                                             }
{ Copyright (c) 1995-2001 Borland Software Corporation                        }
{                                                                             }
{ This file may be distributed and/or modified under the terms of the GNU     }
{ General Public License (GPL) version 2 as published by the Free Software    }
{ Software Foundation and appearing at http://www.borland.com/kylix/gpl.html. }
{                                                                             }
{ Licensees holding a valid Borland No-Nonsense License for this Software may }
{ use this file in accordance with such license, which appears in the file    }
{ license.txt that came with this Software.                                   }
{                                                                             }
{ *************************************************************************** }
{
  $Header: /cvsroot/dwpl/dwpl/source/rtl/sys/Contnrs.pas,v 1.3 2002/10/27 22:48:30 iwache Exp $
  ----------------------------------------------------------------------------

  $Log: Contnrs.pas,v $
  Revision 1.3  2002/10/27 22:48:30  iwache
  CVS tags "Header" and "Log" added.

  ----------------------------------------------------------------------------
}
unit Contnrs;

interface

uses
  SysUtils, Classes, ClassesNew;

type

{ TObjectList class }

  TObjectList = class(TNotifyList)
  private
    FOwnsObjects: Boolean;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function GetItem(Index: Integer): TObject;
    procedure SetItem(Index: Integer; AObject: TObject);
  public
    constructor Create; overload;
    constructor Create(AOwnsObjects: Boolean); overload;

    function Add(AObject: TObject): Integer;
    function Extract(Item: TObject): TObject;
    function Remove(AObject: TObject): Integer;
    function IndexOf(AObject: TObject): Integer;
    function FindInstanceOf(AClass: TClass; AExact: Boolean = True; AStartAt: Integer = 0): Integer;
    procedure Insert(Index: Integer; AObject: TObject);
    function First: TObject;
    function Last: TObject;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
  end;

{ TComponentList class }

  TComponentList = class(TObjectList)
  private
    FNexus: TComponent;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function GetItems(Index: Integer): TComponent;
    procedure SetItems(Index: Integer; AComponent: TComponent);
    procedure HandleFreeNotify(Sender: TObject; AComponent: TComponent);
  public
    destructor Destroy; override;

    function Add(AComponent: TComponent): Integer;
    function Extract(Item: TComponent): TComponent;
    function Remove(AComponent: TComponent): Integer;
    function IndexOf(AComponent: TComponent): Integer;
    function First: TComponent;
    function Last: TComponent;
    procedure Insert(Index: Integer; AComponent: TComponent);
    property Items[Index: Integer]: TComponent read GetItems write SetItems; default;
  end;

{ TClassList class }

  TClassList = class(TNotifyList)
  protected
    function GetItems(Index: Integer): TClass;
    procedure SetItems(Index: Integer; AClass: TClass);
  public
    function Add(AClass: TClass): Integer;
    function Extract(Item: TClass): TClass;
    function Remove(AClass: TClass): Integer;
    function IndexOf(AClass: TClass): Integer;
    function First: TClass;
    function Last: TClass;
    procedure Insert(Index: Integer; AClass: TClass);
    property Items[Index: Integer]: TClass read GetItems write SetItems; default;
  end;

{ TOrdered class }

  TOrderedList = class(TObject)
  private
    FList: TList;
  protected
    procedure PushItem(AItem: Pointer); virtual; abstract;
    function PopItem: Pointer; virtual;
    function PeekItem: Pointer; virtual;
    property List: TList read FList;
  public
    constructor Create;
    destructor Destroy; override;

    function Count: Integer;
    function AtLeast(ACount: Integer): Boolean;
    function Push(AItem: Pointer): Pointer;
    function Pop: Pointer;
    function Peek: Pointer;
  end;

{ TStack class }

  TStack = class(TOrderedList)
  protected
    procedure PushItem(AItem: Pointer); override;
  end;

{ TObjectStack class }

  TObjectStack = class(TStack)
  public
    function Push(AObject: TObject): TObject;
    function Pop: TObject;
    function Peek: TObject;
  end;

{ TQueue class }

  TQueue = class(TOrderedList)
  protected
    procedure PushItem(AItem: Pointer); override;
  end;

{ TObjectQueue class }

  TObjectQueue = class(TQueue)
  public
    function Push(AObject: TObject): TObject;
    function Pop: TObject;
    function Peek: TObject;
  end;

{ Easy access error message }

procedure RaiseListError(const ATemplate: string; const AData: array of const);

implementation

uses
  Math;

{ Easy access error message }

procedure RaiseListError(const ATemplate: string; const AData: array of const);
begin
  raise EListError.CreateFmt(ATemplate, AData);
end;

{ TObjectList }

function TObjectList.Add(AObject: TObject): Integer;
begin
  Result := inherited Add(AObject);
end;

constructor TObjectList.Create;
begin
  inherited Create;
  FOwnsObjects := True;
end;

constructor TObjectList.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

function TObjectList.Extract(Item: TObject): TObject;
begin
  Result := TObject(inherited Extract(Item));
end;

function TObjectList.FindInstanceOf(AClass: TClass; AExact: Boolean;
  AStartAt: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := AStartAt to Count - 1 do
    if (AExact and
        (Items[I].ClassType = AClass)) or
       (not AExact and
        Items[I].InheritsFrom(AClass)) then
    begin
      Result := I;
      break;
    end;
end;

function TObjectList.First: TObject;
begin
  Result := TObject(inherited First);
end;

function TObjectList.GetItem(Index: Integer): TObject;
begin
  Result := inherited Items[Index];
end;

function TObjectList.IndexOf(AObject: TObject): Integer;
begin
  Result := inherited IndexOf(AObject);
end;

procedure TObjectList.Insert(Index: Integer; AObject: TObject);
begin
  inherited Insert(Index, AObject);
end;

function TObjectList.Last: TObject;
begin
  Result := TObject(inherited Last);
end;

procedure TObjectList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if OwnsObjects then
    if Action = lnDeleted then
      TObject(Ptr).Free;
  inherited Notify(Ptr, Action);
end;

function TObjectList.Remove(AObject: TObject): Integer;
begin
  Result := inherited Remove(AObject);
end;

procedure TObjectList.SetItem(Index: Integer; AObject: TObject);
begin
  inherited Items[Index] := AObject;
end;


{ TComponentListNexus }
{ used by TComponentList to get free notification }

type
  TComponentListNexusEvent = procedure(Sender: TObject; AComponent: TComponent) of object;
  TComponentListNexus = class(TComponent)
  private
    FOnFreeNotify: TComponentListNexusEvent;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property OnFreeNotify: TComponentListNexusEvent read FOnFreeNotify write FOnFreeNotify;
  end;

{ TComponentListNexus }

procedure TComponentListNexus.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and Assigned(FOnFreeNotify) then
    FOnFreeNotify(Self, AComponent);
  inherited Notification(AComponent, Operation);
end;

{ TComponentList }

function TComponentList.Add(AComponent: TComponent): Integer;
begin
  Result := inherited Add(AComponent);
end;

destructor TComponentList.Destroy;
begin
  inherited Destroy;
  FNexus.Free;
end;

function TComponentList.Extract(Item: TComponent): TComponent;
begin
  Result := TComponent(inherited Extract(Item));
end;

function TComponentList.First: TComponent;
begin
  Result := TComponent(inherited First);
end;

function TComponentList.GetItems(Index: Integer): TComponent;
begin
  Result := TComponent(inherited Items[Index]);
end;

procedure TComponentList.HandleFreeNotify(Sender: TObject; AComponent: TComponent);
begin
  Extract(AComponent);
end;

function TComponentList.IndexOf(AComponent: TComponent): Integer;
begin
  Result := inherited IndexOf(AComponent);
end;

procedure TComponentList.Insert(Index: Integer; AComponent: TComponent);
begin
  inherited Insert(Index, AComponent);
end;

function TComponentList.Last: TComponent;
begin
  Result := TComponent(inherited Last);
end;

procedure TComponentList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if not Assigned(FNexus) then
  begin
    FNexus := TComponentListNexus.Create(nil);
    TComponentListNexus(FNexus).OnFreeNotify := HandleFreeNotify;
  end;
  case Action of
    lnAdded:
      if Ptr <> nil then
        TComponent(Ptr).FreeNotification(FNexus);
    {lnExtracted,
    lnDeleted:
      if Ptr <> nil then
        TComponent(Ptr).RemoveFreeNotification(FNexus);}
  end;
  inherited Notify(Ptr, Action);
end;

function TComponentList.Remove(AComponent: TComponent): Integer;
begin
  Result := inherited Remove(AComponent);
end;

procedure TComponentList.SetItems(Index: Integer; AComponent: TComponent);
begin
  inherited Items[Index] := AComponent;
end;

{ TClassList }

function TClassList.Add(AClass: TClass): Integer;
begin
  Result := inherited Add(AClass);
end;

function TClassList.Extract(Item: TClass): TClass;
begin
  Result := TClass(inherited Extract(Item));
end;

function TClassList.First: TClass;
begin
  Result := TClass(inherited First);
end;

function TClassList.GetItems(Index: Integer): TClass;
begin
  Result := TClass(inherited Items[Index]);
end;

function TClassList.IndexOf(AClass: TClass): Integer;
begin
  Result := inherited IndexOf(AClass);
end;

procedure TClassList.Insert(Index: Integer; AClass: TClass);
begin
  inherited Insert(Index, AClass);
end;

function TClassList.Last: TClass;
begin
  Result := TClass(inherited Last);
end;

function TClassList.Remove(AClass: TClass): Integer;
begin
  Result := inherited Remove(AClass);
end;

procedure TClassList.SetItems(Index: Integer; AClass: TClass);
begin
  inherited Items[Index] := AClass;
end;

{ TOrderedList }

function TOrderedList.AtLeast(ACount: integer): boolean;
begin
  Result := List.Count >= ACount;
end;

function TOrderedList.Peek: Pointer;
begin
  Result := PeekItem;
end;

function TOrderedList.Pop: Pointer;
begin
  Result := PopItem;
end;

function TOrderedList.Push(AItem: Pointer): Pointer;
begin
  PushItem(AItem);
  Result := AItem;
end;

function TOrderedList.Count: Integer;
begin
  Result := List.Count;
end;

constructor TOrderedList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TOrderedList.Destroy;
begin
  List.Free;
  inherited Destroy;
end;

function TOrderedList.PeekItem: Pointer;
begin
  Result := List[List.Count-1];
end;

function TOrderedList.PopItem: Pointer;
begin
  Result := PeekItem;
  List.Delete(List.Count-1);
end;

{ TStack }

procedure TStack.PushItem(AItem: Pointer);
begin
  List.Add(AItem);
end;

{ TObjectStack }

function TObjectStack.Peek: TObject;
begin
  Result := TObject(inherited Peek);
end;

function TObjectStack.Pop: TObject;
begin
  Result := TObject(inherited Pop);
end;

function TObjectStack.Push(AObject: TObject): TObject;
begin
  Result := TObject(inherited Push(AObject));
end;

{ TQueue }

procedure TQueue.PushItem(AItem: Pointer);
begin
  List.Insert(0, AItem);
end;

{ TObjectQueue }

function TObjectQueue.Peek: TObject;
begin
  Result := TObject(inherited Peek);
end;

function TObjectQueue.Pop: TObject;
begin
  Result := TObject(inherited Pop);
end;

function TObjectQueue.Push(AObject: TObject): TObject;
begin
  Result := TObject(inherited Push(AObject));
end;

end.
