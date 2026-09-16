unit TestTripleQuotes;

interface

uses gnugettext;

resourcestring
  // Valid triple-quoted string, should both have two spaces in front of the text.
  Triple1 = '''
              Triple1 - Line 1 - 2 leading spaces
              Triple1 - Line 2 - 2 leading spaces
            ''';

  // Valid triple-quoted string, with a single line and NO leading spaces
  Triple2 = '''
    Triple2 - With "double" and 'single' quotes, no leading spaces
    ''';

  // Valid triple-quoted string, 3 lines with the middle being a blank line, NO leading spaces
  Triple3 = '''
  Triple 3 - Line 1

  Triple 3 - Line 3
  ''';

  // Corner case confusing strings, which are NOT triple-quoted-strings:
  Regular1 = '''single''';           // Expected: 'single'
  Regular2 = '''hello''' + 'world';  // Expected: 'hello'world
  Regular3 = 'My happy place''' + 's is still my chair';  // Expected: My happy place's is still my chair

const
  MY_CONST = '''
       Triple quotes string, to be fully ignored
       on extraction by dxgettext
       totally not interesting for translation.
       ''';


implementation

procedure TestCode;
var
  lTestStr: String;
begin

  // In gettext call, 3 lines, 1st: no leading spaces / 2nd: 2 leading spaces / 3rd: 4 leading spaces
  lTestStr := _('''
    Translatable Multi line 1 - no leading spaces
      Translatable Multi line 2 - 2 leading spaces
        Translatable Multi line 3 - 4 leading spaces
    ''');

  // Concat of three triple-quotes strings and a variable, but only first two have gettext calls
  // Expected: Two translating items (the first two triple-quoted strings), both with NO leading spaces
  lTestStr := _('''
    The following message was received:
    ''') + lTestStr + _('''
      Please take care of it.
      ''') + '''
	[non translated code string 1234]
	''';

end;


end.