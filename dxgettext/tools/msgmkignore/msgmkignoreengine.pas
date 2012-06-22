unit msgmkignoreengine;

interface

uses
  Classes, poparser;

type
  Tmsgmkignoreengine=
    class
    public
      inputfilename:string;
      outputfilename:string;
      procedure Execute;
    private
      infile:TextFile;
      outfile:TFileStream;
      procedure HandleEntry (entry:TPoEntry);
    end;
    
implementation

uses
  SysUtils,
  gnugettext, ignoredetector;

{ Tmsgmkignoreengine }

procedure Tmsgmkignoreengine.Execute;
var
  pop:TPoParser;
  entry:TPoEntry;
begin
  if inputfilename='' then
    raise Exception.Create (_('No input filename specified.'));
  if outputfilename='' then
    raise Exception.Create (_('No output filename specified.'));
  AssignFile (infile,inputfilename);
  Reset (infile);
  outfile:=TFileStream.Create (outputfilename, fmCreate);
  try
    pop:=TPoParser.Create;
    try
      StreamWrite (outfile,'#  '+_('Ignore list. Remove those that you do want to have translated.'));
      StreamWrite (outfile,sLineBreak);
      StreamWrite (outfile,sLineBreak);
      while true do begin
        entry:=pop.ReadNextEntry(infile);
        if entry=nil then
          break;
        HandleEntry (entry);
      end;
    finally
      FreeAndNil (pop);
    end;
  finally
    FreeAndNil (outfile);
    CloseFile (infile);
  end;
end;

procedure Tmsgmkignoreengine.HandleEntry(entry: TPoEntry);
begin
  if not IsProbablyTranslatable (entry) then
    entry.WriteToStream(outfile);
end;

end.
