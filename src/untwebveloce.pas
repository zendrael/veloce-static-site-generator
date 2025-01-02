//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\
// Veloce static site generator
//   web unit
//   create web application
// @author : Zendrael <zendrael@gmail.com>
// @date   : 2013/10/20
//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\
unit untWebVeloce;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, HTTPDefs, custcgi, custweb,
    //prepare for password reading/writing
    md5,
    //the Content Management System
    untSSG;

type

	TVeloceWeb = Class( TCustomCGIApplication )
		protected
			function InitializeWebHandler: TWebHandler; override;
		public
            //
	end;

	TVeloceCGIHandler = class(TCGIHandler)
		public
			procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
	end;

implementation

{ TVeloceWeb }

function TVeloceWeb.InitializeWebHandler: TWebHandler;
begin
    Result := TVeloceCGIHandler.Create( Self );
end;


{ TVeloceCGIHandler }

procedure TVeloceCGIHandler.HandleRequest(ARequest: TRequest;
	AResponse: TResponse);
begin
	//handle requests
    AResponse.ContentType := 'text/html; charset=utf-8';

    //testing page
    AResponse.Contents.Add('<h1>Veloce Web</h1>');

    //show header and menu

    //handle actions via GET
    case ARequest.QueryFields.Values['action'] of
		'list' : begin
			//get a list of all posts from
		end;

    	'new' : begin
			//
		end;

        'publish' : begin
			//
		end;

        'update' : begin
			//
		end;

        'help' : begin
			//
		end;

        else
			//AResponse.Contents.Add('Welcome!');
            AResponse.Contents.LoadFromFile('../testes/veloce/webinterface/index.html');
	end;

end;

end.

