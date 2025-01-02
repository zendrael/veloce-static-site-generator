//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\
// Veloce static site generator
//   main unit
// create application for console or web with cgi
// @author : Zendrael <zendrael@gmail.com>
// @date   : 2013/10/20
//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\
program veloce;

{$mode objfpc}{$H+}

uses
	{$IFDEF UNIX}{$IFDEF UseCThreads}
	cthreads,
	{$ENDIF}{$ENDIF}
	Classes, SysUtils,
	// units do sistema
	untConsoleVeloce, untWebVeloce;

var
	ConsoleApp: TVeloceConsole;
	WebApp: TVeloceWeb;

begin
    // check which type of app to start
	if( GetEnvironmentVariable('HTTP_HOST') = '' ) then
	begin
		// Running Console Mode
		ConsoleApp := TVeloceConsole.Create(nil);
		ConsoleApp.Title := 'Veloce';
		ConsoleApp.Run;
		ConsoleApp.Free;
	end else
	begin
        // Running Web Mode
		WebApp := TVeloceWeb.Create(nil);
        WebApp.Title := 'Veloce';
        WebApp.Initialize;
		WebApp.Run;
		WebApp.Free;
	end;
end.


