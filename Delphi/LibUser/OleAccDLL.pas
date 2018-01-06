(****************************************************************************
 *
 * Copyright 2016-2017 Tim De Baets
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 ****************************************************************************
 *
 * oleacc.dll API function declarations
 *
 ****************************************************************************)

unit OleAccDLL;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 2007-08-12 ?? 5:49:34 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\WINDOWS\SYSTEM32\OLEACC.DLL (1)
// LIBID: {1EA4DBF0-3C3B-11CF-810C-00AA00389B71}
// LCID: 0
// Helpfile: 
// HelpString: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\System32\stdole2.tlb)
// Errors:
//   Hint: Parameter 'var' of IAccPropServices.SetPropValue changed to 'var_'
//   Hint: Parameter 'var' of IAccPropServices.SetHwndProp changed to 'var_'
//   Hint: Parameter 'var' of IAccPropServices.SetHmenuProp changed to 'var_'
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers.
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Accessibility_TLB;

{$EXTERNALSYM LresultFromObject}
// STDAPI_(LRESULT) LresultFromObject(REFIID riid, WPARAM wParam, LPUNKNOWN punk);
function LResultFromObject(const riid : TIID; wP : WPARAM;
    var unk : IUnknown) : LRESULT; stdcall;

{$EXTERNALSYM ObjectFromLresult}
// STDAPI          ObjectFromLresult(LRESULT lResult, REFIID riid, WPARAM wParam, void** ppvObject);
function ObjectFromLResult(LR : LRESULT; const riid : TIID; wP : WPARAM;
    out vObject) : HRESULT; stdcall;

{$EXTERNALSYM WindowFromAccessibleObject}
// STDAPI          WindowFromAccessibleObject(IAccessible*, HWND* phwnd);
function WindowFromAccessibleObject(Acc : IAccessible;
    var H : HWND) : HRESULT; stdcall;

{$EXTERNALSYM AccessibleObjectFromWindow}
// STDAPI          AccessibleObjectFromWindow(HWND hwnd, DWORD dwId, REFIID riid, void **ppvObject);
function AccessibleObjectFromWindow(H : HWND; dwID : DWORD; const riid : TIID;
    out vObject) : HRESULT; stdcall;

{$EXTERNALSYM AccessibleObjectFromEvent}
// STDAPI          AccessibleObjectFromEvent(HWND hwnd, DWORD dwId, DWORD dwChildId, IAccessible** ppacc, VARIANT* pvarChild);
function AccessibleObjectFromEvent(H : HWND; dwID, dwChildID : DWORD;
    out Acc : IAccessible; var varChild : OleVariant) : HRESULT; stdcall;

{$EXTERNALSYM AccessibleObjectFromPoint}
// STDAPI          AccessibleObjectFromPoint(POINT ptScreen, IAccessible ** ppacc, VARIANT* pvarChild);
function AccessibleObjectFromPoint(Pt : TPoint; out Acc : IAccessible;
    var varChild : OleVariant) : HRESULT; stdcall;

{$EXTERNALSYM CreateStdAccessibleObject}
// STDAPI          CreateStdAccessibleObject(HWND hwnd, LONG idObject, REFIID riid, void** ppvObject);
function CreateStdAccessibleObject(H : HWND; idObject : LongInt;
    const riid : TIID; out vObject) : HRESULT; stdcall;

{$EXTERNALSYM AccessibleChildren}
// STDAPI        AccessibleChildren (IAccessible* paccContainer, LONG iChildStart, LONG cChildren, VARIANT* rgvarChildren,LONG* pcObtained);
function AccessibleChildren(AccContainer : IAccessible;
    iChildStart, cChildren : LongInt; var rgvarChildren : OleVariant;
    var cObtained) : HRESULT; stdcall;

{$EXTERNALSYM GetRoleTextA}
// STDAPI_(UINT)   GetRoleTextA(DWORD lRole, LPSTR lpszRole, UINT cchRoleMax);
function GetRoleTextA(lRole : DWORD; szRole : PChar;
    cchRoleMax : UINT) : UINT; stdcall;
function GetRoleText(lRole : DWORD; szRole : PChar;
    cchRoleMax : UINT) : UINT; stdcall;

{$EXTERNALSYM GetRoleTextW}
// STDAPI_(UINT)   GetRoleTextW(DWORD lRole, LPWSTR lpszRole, UINT cchRoleMax);
function GetRoleTextW(lRole : DWORD; szRole : PChar;
    cchRoleMax : UINT) : UINT; stdcall;

{$EXTERNALSYM GetStateTextA}
// STDAPI_(UINT)   GetStateTextA(DWORD lStateBit, LPSTR lpszState, UINT cchState);
function GetStateTextA(lStateBit : DWORD; szState : PChar;
    cchState : UINT) : UINT; stdcall;
function GetStateText(lStateBit : DWORD; szState : PChar;
    cchState : UINT) : UINT; stdcall;

{$EXTERNALSYM GetStateTextW}
// STDAPI_(UINT)   GetStateTextW(DWORD lStateBit, LPWSTR lpszState, UINT cchState);
function GetStateTextW(lStateBit : DWORD; szState : PChar;
    cchState : UINT) : UINT; stdcall;

implementation

uses ComObj;

const
  OleAccDLLName    = 'oleacc.dll';

function LResultFromObject;             external OleAccDLLName name 'LresultFromObject';
function ObjectFromLResult;             external OleAccDLLName name 'ObjectFromLresult';
function WindowFromAccessibleObject;    external OleAccDLLName name 'WindowFromAccessibleObject';
function AccessibleObjectFromWindow;    external OleAccDLLName name 'AccessibleObjectFromWindow';
function AccessibleObjectFromEvent;     external OleAccDLLName name 'AccessibleObjectFromEvent';
function AccessibleObjectFromPoint;     external OleAccDLLName name 'AccessibleObjectFromPoint';
function CreateStdAccessibleObject;     external OleAccDLLName name 'CreateStdAccessibleObject';
function AccessibleChildren;            external OleAccDLLName name 'AccessibleChildren';
function GetRoleText;                   external OleAccDLLName name 'GetRoleTextA';
function GetRoleTextA;                  external OleAccDLLName name 'GetRoleTextA';
function GetRoleTextW;                  external OleAccDLLName name 'GetRoleTextW';
function GetStateText;                  external OleAccDLLName name 'GetStateTextA';
function GetStateTextA;                 external OleAccDLLName name 'GetStateTextA';
function GetStateTextW;                 external OleAccDLLName name 'GetStateTextW';

end.
