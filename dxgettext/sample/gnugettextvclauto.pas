unit gnugettextvclauto;
(**************************************************************)
(*                                                            *)
(*  (C) Copyright by Lars B. Dybdahl and others               *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241               *)
(*  You may distribute and modify this file as you wish       *)
(*  for free                                                  *)
(*                                                            *)
(*  See http://dybdahl.dk/dxgettext/ for more information     *)
(*                                                            *)
(**************************************************************)

interface

// DO NOT use this on multithreaded applications

// DO NOT include this unit in a project where you use TranslateProperties(self)
// to translate forms. Remove the TranslateProperties() function calls or
// rewrite to use TranslateComponent(self) instead.

// Adding this unit to your project will make all forms auto-translate.
// This includes forms embedded in 3rd party vendor libraries and
// components.

// Basically, this unit just makes sure that TranslateComponent (self) is called
// in every TForm that is created, just after where an OnCreate handler
// would be called. This means that you can still use TP_ functions inside
// OnCreate.




implementation

uses
  Windows, Classes, RTLConsts, SysUtils, TypInfo, gnugettext, Forms,
  Controls;

type
  TCharArray5=array[0..4] of ansichar;
  THook=
    class
    public
      constructor Create (OldProcedure, NewProcedure: pointer);
      destructor Destroy; override;  // Restores unhooked state
      procedure Disable;
      procedure Enable;
    private
      Patch:TCharArray5;
      Original:TCharArray5;
      PatchPosition:PChar;
    end;
  TGGForm=
    class (TCustomForm)
      procedure GGDoCreate;
    end;
  TGGDataModule=
    class (TDataModule)
      procedure GGDoCreate;
    end;

var
  HookFormOnCreate:THook;
  HookDMOnCreate:THook;
  
{ THook }

constructor THook.Create(OldProcedure, NewProcedure: pointer);
var
  offset: integer;
  ov: cardinal;
begin
  PatchPosition:=PChar(OldProcedure);
  offset:=integer(NewProcedure)-integer(OldProcedure)-5;

  Patch[0] := char($E9);
  Patch[1] := char(offset and 255);
  Patch[2] := char((offset shr 8) and 255);
  Patch[3] := char((offset shr 16) and 255);
  Patch[4] := char((offset shr 24) and 255);

  Original[0]:=PatchPosition[0];
  Original[1]:=PatchPosition[1];
  Original[2]:=PatchPosition[2];
  Original[3]:=PatchPosition[3];
  Original[4]:=PatchPosition[4];

  if not VirtualProtect(Pointer(PatchPosition), 5, PAGE_EXECUTE_READWRITE, @ov) then
    RaiseLastOSError;

  Enable;
end;

destructor THook.Destroy;
begin
  Disable;
  inherited;
end;

procedure THook.Disable;
begin
  PatchPosition[0]:=Original[0];
  PatchPosition[1]:=Original[1];
  PatchPosition[2]:=Original[2];
  PatchPosition[3]:=Original[3];
  PatchPosition[4]:=Original[4];
end;

procedure THook.Enable;
begin
  PatchPosition[0]:=Patch[0];
  PatchPosition[1]:=Patch[1];
  PatchPosition[2]:=Patch[2];
  PatchPosition[3]:=Patch[3];
  PatchPosition[4]:=Patch[4];
end;

{ TGGForm }

procedure TGGForm.GGDoCreate;
begin
  HookFormOnCreate.Disable;
  try
    DoCreate;
  finally
    HookFormOnCreate.Enable;
  end;
  DisableAlign;
  DisableAutoRange;
  try
    TranslateComponent (self);
  finally
    EnableAlign;
    EnableAutoRange;
  end;
end;

{ TGGDataModule }

procedure TGGDataModule.GGDoCreate;
begin
  HookDMOnCreate.Disable;
  try
    DoCreate;
  finally
    HookDMOnCreate.Enable;
  end;
  TranslateComponent (self);
end;

initialization
  HookFormOnCreate:=THook.Create (@TGGForm.DoCreate,@TGGForm.GGDoCreate);
  HookDMOnCreate:=THook.Create(@TGGDataModule.DoCreate,@TGGDataModule.GGDoCreate);

finalization
  FreeAndNil (HookDMOnCreate);
  FreeAndNil (HookFormOnCreate);

end.

