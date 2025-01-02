//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\
// Veloce static site generator
//   Static Site Generator unit
//   POST generation
// @author : Zendrael <zendrael@gmail.com>
// @date   : 2014/10/19
//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\
unit untPost;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, RegExpr;
    //teste
    //MarkdownDaringFireball, MarkdownProcessor;

type

	{ TPost }

 	TPost = class
		protected
            //
			strTitle: string;
            strDate : String;
            strTags: TStringList;
            strContentFull: TStringList;
            strContent: TStringList;
			strFileName: string;
            strTemplate : string;
        public
			//
			procedure setTitle( Title: string );
			function getTitle: string;
            function getTitleLink: string;
            procedure setDate( date: string);
			function getDate: string;
			procedure setTags( Tags: string );
			function getTags: string;
			procedure setContent( Content: string );
			function getContent: string;
			procedure setFileName( name: string);
			function getFileName: string;
            procedure setTemplate( name: string);
			function getTemplate: string;

            procedure LoadFile( path, fileName: string );
            function getFullPost: string;
            function getSimplePost: string;
            procedure applyTemplate( strFullTemplate : WideString );
            procedure save( strFullFileName: String );

			function Explode(delimiter:string; source:string) : TStringList;

			constructor Create;
			destructor Destroy; override;
    end;

implementation

{ TPost }

function TPost.Explode(delimiter:string; source:string) : TStringList;
(*
// Name : Explode
// Purpose : each of which is a substring of string formed by splitting
it on
// boundaries formed by the string delim. It does not support PHP's
// 'limit' feature.
// Date : 12 Feb 2001 by Bob Brown (bob....@opus.co.nz)
// Comments : Based on PHP's Explode function (http://www.php.net/explode).
// Returns an array of strings, each of which is a substring of
// string formed by splitting it on boundaries formed by the string //
delimiter.
*)
var
   c : word;
begin
   Result:=TStringList.Create;
   c:=0;
   while source<>'' do
   begin
     if Pos(delimiter,source)>0 then
     begin
       Result.Add(Copy(Source,1,Pos(delimiter,source)-1));
       Delete(Source,1,Length(Result[c])+Length(delimiter));
     end
     else
     begin
       Result.Add( Trim( Source ) );
       Source:='';
     end;
     inc(c);
   end;
end;

procedure TPost.setTitle(Title: string);
begin
   Self.strTitle:= Title;
end;

function TPost.getTitle: string;
begin
	Result:= self.strTitle;
end;

function TPost.getTitleLink: string;
begin
	Result:= '<a href="'+ getFileName +'">'+ self.strTitle + '</a>';
end;

procedure TPost.setDate(date: string);
begin
    self.strDate := date;
end;

function TPost.getDate: string;
begin
	Result := self.strDate;
end;

procedure TPost.setTags(Tags: string);
var
	i: integer;
begin
   self.strTags.Add( Tags );

   //processa cada tag em uma linha e cria os links
   self.strTags:= Explode(',', self.strTags.Text);

   for i:=0 to self.strTags.Count-1 do
   begin
     self.strTags[i] := '<a href="tag-'+ Trim(self.strTags[i]) +'.html">' + Trim(self.strTags[i]) + '</a>&nbsp;';
   end;

end;

function TPost.getTags: string;
begin
    //deve retornar conjunto ul li a
	Result:= self.strTags.Text;
end;

procedure TPost.setContent(Content: string);
begin
	self.strContentFull.Text:= Content;
    self.strContent.Text := Content;
end;

function TPost.getContent: string;
begin
	Result:= self.strContentFull.Text;
end;

procedure TPost.LoadFile(path, fileName: string);
var
    strTheme : TStringList;
    //md : TMarkdownProcessor;
begin
	//initialize vars
    strTheme := TStringList.Create;

    //load template
    strTheme.LoadFromFile( getTemplate + '/' + 'detail.html' );

	//load file content
	strContentFull.LoadFromFile( path + '/' + fileName );

	//separa o nome do arquivo
	setFileName( fileName );

	//prepara cada item se acordo com o arquivo
	setTitle( strContentFull[0] );
    setDate( strContentFull[1] );
	setTags( strContentFull[2] );

	//deleta as primeiras linhas e retorna o resto.
	//a cada delete, o index e o count é alterado,
	//por isso deleto as primeiras 4 linhas através de 0
	strContentFull.Delete(0);
	strContentFull.Delete(0);
	strContentFull.Delete(0);
    strContentFull.Delete(0);
	//post.Delete(3);

    //replace line breaks
	strContentFull.Text:= StringReplace( Trim( strContentFull.text ), sLineBreak, '<br />', [rfreplaceAll]);

    //process markdown
    //strContentFull.Text:= md.process( strContentFull.Text );
    //negrito
    strContentFull.Text:= ReplaceRegExpr('/(\*\*|__)(.*?)\1/',strContentFull.Text,'<strong>$1</strong>',TRUE);

    //replace all starting with template
    strTheme.Text := StringReplace(strTheme.Text, '%%POSTTITLE%%', getTitleLink, [rfreplaceAll]);
    strTheme.Text := StringReplace(strTheme.Text, '%%POSTCONTENT%%', strContentFull.Text, [rfreplaceAll]);
    strTheme.Text := StringReplace(strTheme.Text, '%%POSTDATE%%', getDate, [rfreplaceAll]);
    strTheme.Text := StringReplace(strTheme.Text, '%%POSTTAGS%%', getTags, [rfreplaceAll]);

	//set complete content
    setContent( Trim( strTheme.Text ) );
end;

procedure TPost.setFileName(name: string);
begin
   Delete( name, Pos('.', name), Length( name ) );
   self.strFileName:= name + '.html';
end;

function TPost.getFileName: string;
begin
	Result:= self.strFileName;
end;

procedure TPost.setTemplate(name: string);
begin
    self.strTemplate:= name;
end;

function TPost.getTemplate: string;
begin
	Result := self.strTemplate;
end;

function TPost.getFullPost: string;
begin
     Result := self.strContentFull.Text;
end;

function TPost.getSimplePost: string;
begin
     Result := self.strContent.Text;
end;

procedure TPost.applyTemplate(strFullTemplate: WideString);
begin
  //apply template to the post (post to template exactly)
  strContentFull.Text := StringReplace(strFullTemplate, '%%POSTS%%', getFullPost, [rfreplaceAll]);
  //remove pagination for single post
  strContentFull.Text := StringReplace(strContentFull.Text, '%%PAGINATION%%', '', [rfreplaceAll]);

  strContentFull.Text := StringReplace(strContentFull.Text, '%%POSTTITLE%%', getTitle, [rfreplaceAll]);
end;

procedure TPost.save( strFullFileName: String );
begin
   //save file
   strContentFull.SaveToFile( strFullFileName );
end;

constructor TPost.Create;
begin
	self.strContentFull:= TStringList.Create;
    self.strContent:= TStringList.Create;
	self.strTags:= TStringList.Create;
end;

destructor TPost.Destroy;
begin
	inherited Destroy;
end;

end.

