(****************************************************************************
 *
 * Unit: ComCtrls95
 *
 * Components: TPage95Control, TTab95Control, TTab95Sheet
 *
 * Author: Ryan J. Mills
 *
 * Author Email: rmills@freenet.edmonton.ab.ca
 * Code Status: Freeware
 *
 * Additional Codeing: Daniel Parnell,
 *                     Patrick O'Keeffe,
 *                     Dave Lively (Floating Tabs),
 *                     Earl F. Glynn (Rotation Routines),
 *                     Mark Timmings,
 *                     Andrea Sessa,
 *                     Walter Sciancalepore,
 *                     Flemming Gorzelak,
 *                     Richard Chang,
 *                     Juan Pedro Laencina.
 *
 * Disclaimer: THIS SOFTWARE AND THE ACCOMPANYING FILES ARE DISTRIBUTED
 * "AS IS" AND WITHOUT WARRANTIES AS TO PERFORMANCE OF MERCHANTABILITY OR
 * ANY OTHER WARRANTIES WHETHER EXPRESSED OR IMPLIED.  Because of the
 * various software environments into which this code may be used, NO
 * WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE IS OFFERED.  Good data
 * processing procedure dictates that any program based on this code be
 * thoroughly tested with non-critical data before relying on it.
 * THE USER MUST ASSUME THE ENTIRE RISK OF USING THE ACCOMPANYING CODE.
 *
 * Comments: This code is being released as freeware.  The author asks
 * that if you modify the code that you email a copy of the new source
 * back to him.
 *
 * Appreciation:  Many thanks to Mark Timmings for his many emails
 * reporting bugs in the controls and for adding his personal touch
 * to the control interface.  I appreciate all your help Mark!
 *
 ****************************************************************************)

unit CC95_Register;

interface

uses Classes, sysutils, DsgnIntf, typinfo, ComCtrls95;

{$ifdef ver90}      //remove this line for explict D2 usage
  {$Define delphi2} //Create .DCU for D2
{$endif}            //remove this line for explict D2 usage

{$ifdef ver110}     //This is for BCB3  Do not remove!
   {$Define delphi4}
   {$Define BCB3}
{$endif}

{$ifdef ver120}     //remove this line for explict D4 usage
  {$Define delphi4} //Create .DCU for D4
{$endif}            //remove this line for explict D4 usage

{$ifdef ver130}
  {$define Delphi4}
{$endif}

type
  TPage95ControlEditor = class(TComponentEditor)
  private
    procedure NewPage;
    procedure ChangePage(Forward:boolean);
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(index:integer); override;
    procedure Edit; Override;
  end;


procedure Register;

implementation

// TPage95Control Component Editor

function TPage95ControlEditor.GetVerb(index:integer):string;
begin
     case index of
          0:result := 'New Page';
          1:result := 'Next Page';
          2:result := 'Previous Page';
     end;
end;

function TPage95ControlEditor.GetVerbCount:integer;
begin
     result := 3;
end;

procedure TPage95ControlEditor.Edit;
var
   eventname : string;
   changeevent : TNotifyEvent;
   ppi: PPropInfo;
   pagecontrol : tpage95control;
begin
     if component is ttab95sheet then
       pagecontrol := ttab95sheet(component).pagecontrol
     else
       pagecontrol := tpage95control(component);

     changeevent := pagecontrol.OnChange;
     if assigned(changeevent) then
        eventname := designer.getmethodname(tmethod(changeevent))
     else
     begin
          eventname := pagecontrol.name + 'Change';
          ppi := GetPropInfo( pagecontrol.classinfo,'OnChange');
          {$IfDef delphi2}
          changeevent := tnotifyevent(designer.createmethod(eventname,gettypedata(ppi.proptype)));
          {$Else}
          changeevent := tnotifyevent(designer.createmethod(eventname,gettypedata(ppi.proptype^)));
          {$EndIF}
          pagecontrol.onchange := changeevent;
          designer.modified;
     end;
     designer.showmethod(eventname);
end;

procedure TPage95ControlEditor.ExecuteVerb(index:integer);
begin
     case index of
          0:NewPage;
          1:ChangePage(true); //Next Page
          2:ChangePage(False); //Previous Page
     end;
end;

procedure TPage95ControlEditor.NewPage;
var
   pagecontrol : tpage95control;
   page : ttab95sheet;
   {$ifndef delphi4}
   designer: tformdesigner;
   {$endif}
begin
     if component is ttab95sheet then
       pagecontrol := ttab95sheet(component).pagecontrol
     else
       pagecontrol := tpage95control(component);

     if pagecontrol <> nil then
     begin
       {$ifndef delphi4}
       designer := self.designer;
       {$endif}
          page := ttab95sheet.Create(designer.form);
          try
            page.name := designer.uniquename(ttab95sheet.classname);
            page.parent := pagecontrol;
            page.pagecontrol := pagecontrol;
            page.caption := page.name;
          except
            page.free;
            raise;
          end;
          pagecontrol.activepage := page;
          designer.selectcomponent(page);
          designer.modified;
     end;
end;

procedure TPage95ControlEditor.ChangePage(forward:boolean);
var
   pagecontrol : tpage95control;
   page : ttab95sheet;
   {$ifndef delphi4}
   designer: tformdesigner;
   {$endif}
begin
     if component is ttab95sheet then
       pagecontrol := ttab95sheet(component).pagecontrol
     else
       pagecontrol := tpage95control(component);
     if pagecontrol <> nil then
     begin
       {$ifndef delphi4}
       designer := self.designer;
       {$endif}
       page := pagecontrol.findnextpage(pagecontrol.activepage, forward, false);
       if (page <> nil) and (page <> pagecontrol.activepage) then
       begin
         pagecontrol.activepage := page;
         if component is ttab95sheet then designer.selectcomponent(page);
         designer.modified;
       end;
     end;
end;

// TPage95OnChangeEditor

procedure Register;
begin
{$IFDEF delphi2}
     RegisterComponents('Win95',[TTab95Control, TPage95Control]);
{$ELSE}
     RegisterComponents('Win32',[TTab95Control, TPage95Control]);
{$ENDIF}
     RegisterClass(TTab95Sheet);
     RegisterComponentEditor(TPage95Control,TPage95ControlEditor);
     RegisterComponentEditor(TTab95Sheet,TPage95ControlEditor);
end;

end.
 