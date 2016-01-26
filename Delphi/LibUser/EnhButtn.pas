//***********************************************************************
// Delphi 2.0 only
// EnhButton derives TButton95, TCheckBox95 and TRadioButton95 from TButton,
// TCheckBox and TRadioButton and adds all the Windows 95 formatting
// functionality Borland forgot when they implemented these components.
//
// Added properties
// All components:
//  HorAlignment : How the Caption of the button is aligned horizontally
//  VerAlignment : How the Caption of the button is aligned vertically
//  Picture      : Display a bitmap or an icon instead of the Caption
//  WordWrap     : Wrap the text if the control is too narrow
// RadioButton95 and Checkbox95 only:
//  LikeButton   : Make the Checkbox or Radiobutton look like a pushbutton
//
//*********************************************************************
// History:
//   22/1/97       Initial release
//   23/1/97       Fixed a bug that caused Delphi to lock up occasionaly
//                 if TButton95 was dropped on a form
//   24/1/94       Added TRadioButton95 and TCheckBox95 when I found out
//                 that an existing component that implemented these two
//                 didn't have a picture property
//***********************************************************************
// Ver 1.3
// Copyright(c) 1997 Arjen Broeze
// e-mail : arjen@shear.iaf.nl
//***********************************************************************
unit EnhButtn;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  THorizontalAlignment = (haLeft, haCenter, haRight);
  TVerticalAlignment   = (vaTop, vaCenter, vaBottom);

  TButton95 = class(TButton)
  private
    { Private declarations }
    FStyle   : LongInt;
    FHorAlign: THorizontalAlignment;
    FVerAlign: TVerticalAlignment;
    FWordWrap: Boolean;
    FPicture : TPicture;
    procedure SetHorAlignment(Value: THorizontalAlignment);
    procedure SetVerAlignment(Value: TVerticalAlignment);
    procedure SetWordWrap(Value: Boolean);
    procedure SetPicture(Value: TPicture);
  protected
    { Protected declarations }
    procedure CheckStyle(Flag: Word);
    procedure ChangeStyle;
    procedure SendPicture;
    procedure PictureChanged(Sender: TObject); virtual;
    procedure CreateWnd; override;
  public
    { Public declarations }
    procedure CreateParams(var Params: TCreateParams); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property HorAlignment: THorizontalAlignment read FHorAlign write SetHorAlignment default haCenter;
    property Picture     : TPicture             read FPicture  write SetPicture;
    property VerAlignment: TVerticalAlignment   read FVerAlign write SetVerAlignment default vaCenter;
    property WordWrap    : Boolean              read FWordWrap write SetWordWrap default False;
  end;

  TRadioButton95 = class(TRadioButton)
  private
    { Private declarations }
    FStyle     : LongInt;
    FHorAlign  : THorizontalAlignment;
    FLikeButton: Boolean;
    FPicture   : TPicture;
    FVerAlign  : TVerticalAlignment;
    FWordWrap  : Boolean;
    procedure SetHorAlignment(Value: THorizontalAlignment);
    procedure SetLikeButton(Value: Boolean);
    procedure SetPicture(Value: TPicture);
    procedure SetVerAlignment(Value: TVerticalAlignment);
    procedure SetWordWrap(Value: Boolean);
  protected
    { Protected declarations }
    procedure CheckStyle(Flag: Word);
    procedure ChangeStyle;
    procedure SendPicture;
    procedure PictureChanged(Sender: TObject); virtual;
    procedure CreateWnd; override;
  public
    { Public declarations }
    procedure CreateParams(var Params: TCreateParams); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property HorAlignment: THorizontalAlignment read FHorAlign   write SetHorAlignment default haLeft;
    property LikeButton  : Boolean              read FLikeButton write SetLikeButton default False;
    property Picture     : TPicture             read FPicture    write SetPicture;
    property VerAlignment: TVerticalAlignment   read FVerAlign   write SetVerAlignment default vaCenter;
    property WordWrap    : Boolean              read FWordWrap   write SetWordWrap default False;
  end;

  TCheckBox95 = class(TCheckBox)
  private
    { Private declarations }
    FStyle     : LongInt;
    FHorAlign  : THorizontalAlignment;
    FLikeButton: Boolean;
    FPicture   : TPicture;
    FVerAlign  : TVerticalAlignment;
    FWordWrap  : Boolean;
    procedure SetHorAlignment(Value: THorizontalAlignment);
    procedure SetLikeButton(Value: Boolean);
    procedure SetPicture(Value: TPicture);
    procedure SetVerAlignment(Value: TVerticalAlignment);
    procedure SetWordWrap(Value: Boolean);
  protected
    { Protected declarations }
    procedure CheckStyle(Flag: Word);
    procedure ChangeStyle;
    procedure SendPicture;
    procedure PictureChanged(Sender: TObject); virtual;
    procedure CreateWnd; override;
  public
    { Public declarations }
    procedure CreateParams(var Params: TCreateParams); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property HorAlignment: THorizontalAlignment read FHorAlign   write SetHorAlignment default haLeft;
    property LikeButton  : Boolean              read FLikeButton write SetLikeButton default False;
    property Picture     : TPicture             read FPicture    write SetPicture;
    property VerAlignment: TVerticalAlignment   read FVerAlign   write SetVerAlignment default vaCenter;
    property WordWrap    : Boolean              read FWordWrap   write SetWordWrap default False;
  end;

procedure Register;

implementation

{ TButton95 }
constructor TButton95.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHorAlign := haCenter;
  FVerAlign := vaCenter;
  FWordWrap := False;
  FStyle := 0;
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
end;

procedure TButton95.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
   begin
     if WordWrap then Style := Style or BS_MULTILINE;
     case HorAlignment of
       haLeft  : Style := Style or BS_LEFT;
       haCenter: Style := Style or BS_CENTER;
       haRight : Style := Style or BS_RIGHT;
     end;
     case VerAlignment of
       vaTop   : Style := Style or BS_TOP;
       vaCenter: Style := Style or BS_VCENTER;
       vaBottom: Style := Style or BS_BOTTOM;
     end;
     if (FPicture<>nil) and (FPicture.Graphic<>nil) then
      begin
        if FPicture.Graphic is TBitmap then
         Style := Style or BS_BITMAP
        else if FPicture.Graphic is TIcon then
         Style := Style or BS_ICON
        else
         Style := Style or BS_TEXT;
      end
     else
      Style := Style or BS_TEXT;
     FStyle := Style;
   end;
end;

procedure TButton95.CreateWnd;
begin
  inherited CreateWnd;
  SendPicture;
end;

destructor TButton95.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

procedure TButton95.SetHorAlignment(Value: THorizontalAlignment);
begin
  if Value<>FHorAlign then
   begin
     FHorAlign := Value;
     ChangeStyle;
   end;
end;

procedure TButton95.SetVerAlignment(Value: TVerticalAlignment);
begin
  if Value<>FVerAlign then
   begin
     FVerAlign := Value;
     ChangeStyle;
   end;
end;

procedure TButton95.SetWordWrap(Value: Boolean);
begin
  if Value<>FWordWrap then
   begin
     FWordWrap := Value;
     ChangeStyle;
   end;
end;

procedure TButton95.SendPicture;
begin
  if (FPicture.Graphic is TBitmap)  then
   Perform(BM_SETIMAGE, IMAGE_BITMAP, FPicture.Bitmap.Handle)
  else if FPicture.Graphic is TIcon then
   Perform(BM_SETIMAGE, IMAGE_ICON, FPicture.Icon.Handle);
end;

procedure TButton95.ChangeStyle;
begin
  RecreateWnd;
  SendPicture;
end;

procedure TButton95.CheckStyle(Flag: Word);
begin
  if (FStyle and Flag)=0 then ChangeStyle else SendPicture;
end;

procedure TButton95.PictureChanged(Sender: TObject);
begin
  if FPicture.Graphic is TBitmap then CheckStyle(BS_BITMAP)
  else if FPicture.Graphic is TIcon then CheckStyle(BS_ICON)
  else CheckStyle(BS_TEXT);
end;

procedure TButton95.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

{ TRadioButton95 }
constructor TRadioButton95.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHorAlign := haLeft;
  FVerAlign := vaCenter;
  FWordWrap := False;
  FStyle := 0;
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
end;

procedure TRadioButton95.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
   begin
     if WordWrap then Style := Style or BS_MULTILINE;
     if LikeButton then Style := Style or BS_PUSHLIKE;
     case HorAlignment of
       haLeft  : Style := Style or BS_LEFT;
       haCenter: Style := Style or BS_CENTER;
       haRight : Style := Style or BS_RIGHT;
     end;
     case VerAlignment of
       vaTop   : Style := Style or BS_TOP;
       vaCenter: Style := Style or BS_VCENTER;
       vaBottom: Style := Style or BS_BOTTOM;
     end;
     if (FPicture<>nil) and (FPicture.Graphic<>nil) then
      begin
        if FPicture.Graphic is TBitmap then
         Style := Style or BS_BITMAP
        else if FPicture.Graphic is TIcon then
         Style := Style or BS_ICON
        else
         Style := Style or BS_TEXT;
      end
     else
      Style := Style or BS_TEXT;
     FStyle := Style;
   end;
end;

procedure TRadioButton95.CreateWnd;
begin
  inherited CreateWnd;
  SendPicture;
end;

destructor TRadioButton95.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

procedure TRadioButton95.SetLikeButton(Value: Boolean);
begin
  if Value<>FLikeButton then
   begin
     FLikeButton := Value;
     ChangeStyle;
   end;
end;

procedure TRadioButton95.SetHorAlignment(Value: THorizontalAlignment);
begin
  if Value<>FHorAlign then
   begin
     FHorAlign := Value;
     ChangeStyle;
   end;
end;

procedure TRadioButton95.SetVerAlignment(Value: TVerticalAlignment);
begin
  if Value<>FVerAlign then
   begin
     FVerAlign := Value;
     ChangeStyle;
   end;
end;

procedure TRadioButton95.SetWordWrap(Value: Boolean);
begin
  if Value<>FWordWrap then
   begin
     FWordWrap := Value;
     ChangeStyle;
   end;
end;

procedure TRadioButton95.SendPicture;
begin
  if (FPicture.Graphic is TBitmap)  then
   Perform(BM_SETIMAGE, IMAGE_BITMAP, FPicture.Bitmap.Handle)
  else if FPicture.Graphic is TIcon then
   Perform(BM_SETIMAGE, IMAGE_ICON, FPicture.Icon.Handle);
end;

procedure TRadioButton95.ChangeStyle;
begin
  RecreateWnd;
  SendPicture;
end;

procedure TRadioButton95.CheckStyle(Flag: Word);
begin
  if (FStyle and Flag)=0 then ChangeStyle else SendPicture;
end;

procedure TRadioButton95.PictureChanged(Sender: TObject);
begin
  if FPicture.Graphic is TBitmap then CheckStyle(BS_BITMAP)
  else if FPicture.Graphic is TIcon then CheckStyle(BS_ICON)
  else CheckStyle(BS_TEXT);
end;

procedure TRadioButton95.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

{ TCheckBox95 }
constructor TCheckBox95.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHorAlign := haLeft;
  FVerAlign := vaCenter;
  FWordWrap := False;
  FStyle := 0;
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
end;

procedure TCheckBox95.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
   begin
     if WordWrap then Style := Style or BS_MULTILINE;
     if LikeButton then Style := Style or BS_PUSHLIKE;
     case HorAlignment of
       haLeft  : Style := Style or BS_LEFT;
       haCenter: Style := Style or BS_CENTER;
       haRight : Style := Style or BS_RIGHT;
     end;
     case VerAlignment of
       vaTop   : Style := Style or BS_TOP;
       vaCenter: Style := Style or BS_VCENTER;
       vaBottom: Style := Style or BS_BOTTOM;
     end;
     if (FPicture<>nil) and (FPicture.Graphic<>nil) then
      begin
        if FPicture.Graphic is TBitmap then
         Style := Style or BS_BITMAP
        else if FPicture.Graphic is TIcon then
         Style := Style or BS_ICON
        else
         Style := Style or BS_TEXT;
      end
     else
      Style := Style or BS_TEXT;
     FStyle := Style;
   end;
end;

procedure TCheckBox95.CreateWnd;
begin
  inherited CreateWnd;
  SendPicture;
end;

destructor TCheckBox95.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

procedure TCheckBox95.SetLikeButton(Value: Boolean);
begin
  if Value<>FLikeButton then
   begin
     FLikeButton := Value;
     ChangeStyle;
   end;
end;

procedure TCheckBox95.SetHorAlignment(Value: THorizontalAlignment);
begin
  if Value<>FHorAlign then
   begin
     FHorAlign := Value;
     ChangeStyle;
   end;
end;

procedure TCheckBox95.SetVerAlignment(Value: TVerticalAlignment);
begin
  if Value<>FVerAlign then
   begin
     FVerAlign := Value;
     ChangeStyle;
   end;
end;

procedure TCheckBox95.SetWordWrap(Value: Boolean);
begin
  if Value<>FWordWrap then
   begin
     FWordWrap := Value;
     ChangeStyle;
   end;
end;

procedure TCheckBox95.SendPicture;
begin
  if (FPicture.Graphic is TBitmap)  then
   Perform(BM_SETIMAGE, IMAGE_BITMAP, FPicture.Bitmap.Handle)
  else if FPicture.Graphic is TIcon then
   Perform(BM_SETIMAGE, IMAGE_ICON, FPicture.Icon.Handle);
end;

procedure TCheckBox95.ChangeStyle;
begin
  RecreateWnd;
  SendPicture;
end;

procedure TCheckBox95.CheckStyle(Flag: Word);
begin
  if (FStyle and Flag)=0 then ChangeStyle else SendPicture;
end;

procedure TCheckBox95.PictureChanged(Sender: TObject);
begin
  if FPicture.Graphic is TBitmap then CheckStyle(BS_BITMAP)
  else if FPicture.Graphic is TIcon then CheckStyle(BS_ICON)
  else CheckStyle(BS_TEXT);
end;

procedure TCheckBox95.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure Register;
begin
  RegisterComponents('Win95', [TButton95, TRadioButton95, TCheckBox95]);
end;

end.
