unit Unit5;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, OpenGL, Math;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    DC: HDC;
    RC: HGLRC;
    Angle: GLfloat;
    procedure InitOpenGL;
    procedure SetupProjection;
    procedure RenderScene;
    procedure SetDCPixelFormat(hdc: HDC);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.SetDCPixelFormat(hdc: HDC);
var
  pfd: TPixelFormatDescriptor;
  pf: Integer;
begin
  FillChar(pfd, SizeOf(pfd), 0);
  pfd.nSize := SizeOf(pfd);
  pfd.nVersion := 1;
  pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  pfd.iPixelType := PFD_TYPE_RGBA;
  pfd.cColorBits := 24;
  pfd.cDepthBits := 16;
  pfd.iLayerType := PFD_MAIN_PLANE;
  pf := ChoosePixelFormat(hdc, @pfd);
  SetPixelFormat(hdc, pf, @pfd);
end;

procedure TForm1.InitOpenGL;
begin
  DC := GetDC(Handle);
  SetDCPixelFormat(DC);
  RC := wglCreateContext(DC);
  wglMakeCurrent(DC, RC);

  glClearColor(0.02, 0.05, 0.15, 1.0);

  glEnable(GL_DEPTH_TEST);
  glShadeModel(GL_SMOOTH);
  SetupProjection;
end;

procedure TForm1.SetupProjection;
var
  aspect: Double;
begin
  if ClientHeight = 0 then aspect := 1 else aspect := ClientWidth / ClientHeight;
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(45.0, aspect, 0.1, 100.0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure TForm1.RenderScene;
begin
  if (RC = 0) or (DC = 0) then Exit;
  wglMakeCurrent(DC, RC);

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;
  glTranslatef(0.0, 0.0, -6.0);
  glRotatef(Angle, 1.0, 1.0, 0.0);

  glBegin(GL_QUADS);

  // === Передня грань ===
  glColor3f(1.0, 0.0, 0.0);
  glVertex3f(-1.0, 1.0,  1.0);
  glVertex3f( 1.0, 1.0,  1.0);
  glColor3f(1.0, 1.0, 0.0);
  glVertex3f( 1.0, -1.0,  1.0);
  glVertex3f(-1.0, -1.0,  1.0);

  // === Задня грань ===
  glColor3f(1.0, 0.0, 0.0);
  glVertex3f( 1.0, 1.0, -1.0);
  glVertex3f(-1.0, 1.0, -1.0);
  glColor3f(1.0, 1.0, 0.0);
  glVertex3f(-1.0, -1.0, -1.0);
  glVertex3f( 1.0, -1.0, -1.0);

  // === Ліва грань ===
  glColor3f(1.0, 0.0, 0.0);
  glVertex3f(-1.0, 1.0, -1.0);
  glVertex3f(-1.0, 1.0,  1.0);
  glColor3f(1.0, 1.0, 0.0);
  glVertex3f(-1.0, -1.0,  1.0);
  glVertex3f(-1.0, -1.0, -1.0);

  // === Права грань ===
  glColor3f(1.0, 0.0, 0.0);
  glVertex3f(1.0, 1.0,  1.0);
  glVertex3f(1.0, 1.0, -1.0);
  glColor3f(1.0, 1.0, 0.0);
  glVertex3f(1.0, -1.0, -1.0);
  glVertex3f(1.0, -1.0,  1.0);

  // === Верхня грань ===
  glColor3f(1.0, 0.0, 0.0);
  glVertex3f(-1.0, 1.0, -1.0);
  glVertex3f( 1.0, 1.0, -1.0);
  glVertex3f( 1.0, 1.0,  1.0);
  glVertex3f(-1.0, 1.0,  1.0);

  // === Нижня грань ===
  glColor3f(1.0, 1.0, 0.0);
  glVertex3f(-1.0, -1.0, -1.0);
  glVertex3f( 1.0, -1.0, -1.0);
  glVertex3f( 1.0, -1.0,  1.0);
  glVertex3f(-1.0, -1.0,  1.0);

  glEnd;

  SwapBuffers(DC);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitOpenGL;
  Angle := 0.0;
  Timer1.Interval := 16;
  Timer1.Enabled := True;
  RenderScene;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if RC <> 0 then
  begin
    wglMakeCurrent(0, 0);
    wglDeleteContext(RC);
    RC := 0;
  end;
  if DC <> 0 then
  begin
    ReleaseDC(Handle, DC);
    DC := 0;
  end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if RC = 0 then Exit;
  wglMakeCurrent(DC, RC);
  glViewport(0, 0, ClientWidth, ClientHeight);
  SetupProjection;
  RenderScene;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Angle := Angle + 0.7;
  if Angle >= 360.0 then Angle := Angle - 360.0;
  RenderScene;
end;

end.

