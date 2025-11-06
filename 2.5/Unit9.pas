unit Unit9;

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
    ColorPhase: GLfloat;
    procedure InitOpenGL;
    procedure SetupProjection;
    procedure RenderScene;
    procedure SetDCPixelFormat(hdc: HDC);
    procedure DrawCylinder;
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
const
  LightPos: array[0..3] of GLfloat = (2.0, 2.0, 3.0, 1.0);
begin
  DC := GetDC(Handle);
  SetDCPixelFormat(DC);
  RC := wglCreateContext(DC);
  wglMakeCurrent(DC, RC);

  glClearColor(0.0, 0.0, 0.05, 1.0);
  glEnable(GL_DEPTH_TEST);
  glShadeModel(GL_SMOOTH);

  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glEnable(GL_COLOR_MATERIAL);

  glLightfv(GL_LIGHT0, GL_POSITION, @LightPos);

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

procedure TForm1.DrawCylinder;
const
  n = 200;
  h = 2.0;
  r = 1.0;
var
  i: Integer;
  fi, dfi: GLfloat;
  r1, g1, b1, r2, g2, b2: GLfloat;
begin
  dfi := 2 * Pi / n;


  r1 := 0.0 + 0.5 * Sin(ColorPhase);
  g1 := 0.8 + 0.2 * Sin(ColorPhase + Pi/2);
  b1 := 0.7 + 0.3 * Sin(ColorPhase + Pi);

  r2 := 0.9 + 0.1 * Sin(ColorPhase + Pi/3);
  g2 := 0.3 + 0.5 * Sin(ColorPhase + 2*Pi/3);
  b2 := 1.0 + 0.0 * Sin(ColorPhase + Pi/4);

  glBegin(GL_QUAD_STRIP);
  for i := 0 to n do
  begin
    fi := i * dfi;
    glNormal3f(Cos(fi), Sin(fi), 0.0);

    glColor3f(r2, g2, b2);
    glVertex3f(r * Cos(fi), r * Sin(fi), h/2);

    glColor3f(r1, g1, b1);
    glVertex3f(r * Cos(fi), r * Sin(fi), -h/2);
  end;
  glEnd;

  glBegin(GL_TRIANGLE_FAN);
  glNormal3f(0.0, 0.0, 1.0);
  glColor3f(r2, g2, b2);
  glVertex3f(0.0, 0.0, h/2);
  for i := 0 to n do
  begin
    fi := i * dfi;
    glVertex3f(r * Cos(fi), r * Sin(fi), h/2);
  end;
  glEnd;

  glBegin(GL_TRIANGLE_FAN);
  glNormal3f(0.0, 0.0, -1.0);
  glColor3f(r1, g1, b1);
  glVertex3f(0.0, 0.0, -h/2);
  for i := 0 to n do
  begin
    fi := i * dfi;
    glVertex3f(r * Cos(fi), r * Sin(fi), -h/2);
  end;
  glEnd;
end;

procedure TForm1.RenderScene;
begin
  if (RC = 0) or (DC = 0) then Exit;
  wglMakeCurrent(DC, RC);

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;
  glTranslatef(0.0, 0.0, -6.0);
  glRotatef(Angle, 1.0, 1.0, 0.0);

  DrawCylinder;

  SwapBuffers(DC);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitOpenGL;
  Angle := 0.0;
  ColorPhase := 0.0;
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
  RenderScene;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Angle := Angle + 0.6;
  if Angle >= 360.0 then Angle := Angle - 360.0;

  ColorPhase := ColorPhase + 0.02;

  RenderScene;
end;

end.

