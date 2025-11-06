unit Unit14;

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
    TimePhase: GLfloat;
    procedure InitOpenGL;
    procedure SetupProjection;
    procedure RenderScene;
    procedure SetDCPixelFormat(hdc: HDC);
    procedure DrawDynamicCylinder;
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

procedure TForm1.DrawDynamicCylinder;
const
  n = 200;
  baseHeight = 3.0;
  baseRadius = 1.0;
var
  i: Integer;
  fi, dfi: GLfloat;
  x, y, z: GLfloat;
  rColor, gColor, bColor: GLfloat;
  radiusFactor: GLfloat;
begin
  dfi := 2 * Pi / n;

  glBegin(GL_QUAD_STRIP);
  for i := 0 to n do
  begin
    fi := i * dfi;

    radiusFactor := 0.95 + 0.05 * Sin(fi*6 + TimePhase);

    x := baseRadius * radiusFactor * Cos(fi);
    y := baseRadius * radiusFactor * Sin(fi);


    z := -baseHeight/2;
    rColor := 0.5 + 0.5 * Sin(ColorPhase + fi*3);
    gColor := 0.5 + 0.5 * Sin(ColorPhase + fi*5 + Pi/2);
    bColor := 0.5 + 0.5 * Sin(ColorPhase + fi*7 + Pi);

    glColor3f(rColor, gColor, bColor);
    glNormal3f(Cos(fi), Sin(fi), 0.0);
    glVertex3f(x, y, z);

    z := baseHeight/2;
    rColor := 0.5 + 0.5 * Sin(ColorPhase + fi*4 + Pi/3);
    gColor := 0.5 + 0.5 * Sin(ColorPhase + fi*6 + Pi/4);
    bColor := 0.5 + 0.5 * Sin(ColorPhase + fi*8 + Pi/2);

    glColor3f(rColor, gColor, bColor);
    glVertex3f(x, y, z);
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

  DrawDynamicCylinder;

  SwapBuffers(DC);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitOpenGL;
  Angle := 0.0;
  ColorPhase := 0.0;
  TimePhase := 0.0;
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
  TimePhase := TimePhase + 0.03;

  RenderScene;
end;

end.

