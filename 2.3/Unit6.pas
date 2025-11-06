unit Unit6;

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
    procedure VertexColor(kx, ky, kz: GLfloat);
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

  glClearColor(0.05, 0.0, 0.1, 1.0);
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

procedure TForm1.VertexColor(kx, ky, kz: GLfloat);
var
  t, r, g, b: GLfloat;
begin
  t := Angle / 40;
  r := Abs(Sin(t * kx));
  g := Abs(Cos(t * ky));
  b := Abs(Sin(t * kz));
  glColor3f(r, g, b);
end;

procedure TForm1.RenderScene;
begin
  if (RC = 0) or (DC = 0) then Exit;
  wglMakeCurrent(DC, RC);

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;
  glTranslatef(0.0, 0.0, -7.0);
  glRotatef(Angle, 1.0, 1.0, 0.0);

  glBegin(GL_QUADS);

  VertexColor(1.0, 0.7, 1.3); glVertex3f( 1,  1,  1);
  VertexColor(1.2, 1.5, 0.9); glVertex3f(-1,  1,  1);
  VertexColor(1.8, 0.8, 1.1); glVertex3f(-1, -1,  1);
  VertexColor(0.5, 1.3, 1.7); glVertex3f( 1, -1,  1);

  VertexColor(1.1, 0.9, 1.6); glVertex3f( 1,  1, -1);
  VertexColor(1.4, 1.2, 0.6); glVertex3f(-1,  1, -1);
  VertexColor(0.7, 1.9, 1.4); glVertex3f(-1, -1, -1);
  VertexColor(1.5, 1.7, 1.0); glVertex3f( 1, -1, -1);

  VertexColor(1.3, 0.5, 0.8); glVertex3f( 1,  1,  1);
  VertexColor(1.0, 1.2, 1.8); glVertex3f( 1,  1, -1);
  VertexColor(0.6, 1.8, 1.4); glVertex3f( 1, -1, -1);
  VertexColor(1.5, 0.9, 0.7); glVertex3f( 1, -1,  1);

  VertexColor(1.7, 1.0, 0.6); glVertex3f(-1,  1,  1);
  VertexColor(1.2, 1.4, 1.0); glVertex3f(-1,  1, -1);
  VertexColor(0.8, 1.1, 1.6); glVertex3f(-1, -1, -1);
  VertexColor(1.4, 0.7, 1.3); glVertex3f(-1, -1,  1);

  VertexColor(1.8, 1.1, 1.2); glVertex3f( 1,  1,  1);
  VertexColor(1.3, 1.4, 0.8); glVertex3f(-1,  1,  1);
  VertexColor(1.0, 1.7, 1.3); glVertex3f(-1,  1, -1);
  VertexColor(1.5, 1.0, 0.9); glVertex3f( 1,  1, -1);

  VertexColor(1.9, 0.9, 1.4); glVertex3f( 1, -1,  1);
  VertexColor(1.6, 1.1, 1.0); glVertex3f(-1, -1,  1);
  VertexColor(1.3, 1.2, 1.8); glVertex3f(-1, -1, -1);
  VertexColor(1.0, 0.8, 1.5); glVertex3f( 1, -1, -1);

  glEnd;
  SwapBuffers(DC);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitOpenGL;
  Angle := 0.0;
  Timer1.Interval := 16;
  Timer1.Enabled := True;
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
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Angle := Angle + 1.0;
  if Angle > 360 then Angle := 0;
  RenderScene;
end;

end.

