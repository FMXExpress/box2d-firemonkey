unit UOpenGLCanvas;

{ This unit is written by Qianyuan Wang. One version is contributed to CnPack(www.cnpack.org) and this version
   doesn't require you install the component. 

  http://hi.baidu.com/wqyfavor
  wqyfavor@qq.com
  QQ: 466798985}

interface
{$I OpenGLCanvas.inc}

uses
   Math,
   Windows,
   Classes,
   Controls,
   Graphics,
   OpenGL,
   ExtCtrls;

const
   TransformStackTop = 10;
   ListNestLevel = 9;
   MaxChar = 128;

type
   ARGB = type Cardinal;
   // Use TARGB(ARGB) to quick access elements of an ARGB color
   TARGB = packed record
      blue, green, red, alpha: Byte;
   end;

   PSingleArray = ^TSingleArray;
   TSingleArray = array of Single;
   TColorVector = array[0..3] of Single;
   TVector4f = array[0..3] of Single;
   PMatrix4f = ^TMatrix4f;
   TMatrix4f = array[0..3] of TVector4f;

   PGLPointF = ^TGLPointF;
   TGLPointF = packed record
      X: Single;
      Y: Single;
   end;

   PGLPointsF = ^TGLPointsF;
   TGLPointsF = array of TGLPointF;

   PGLPointI = ^TGLPointI;
   TGLPointI = TPoint;

   PGLPointsI = ^TGLPointsI;
   TGLPointsI = array of TGLPointI;

   TTransformType = (ttScale, ttTranslate, ttRotate);
   TTransformData = record
      TransformType: TTransformType;
      var1, var2: Single;
   end;

   TLineStippleStyle = (lssSolid, lssDash, lssDashDot, lssDashDotDot, lssDot);

   TGLCanvas = class;
   TAfterRendering = procedure(Sender: TGLCanvas) of object;
   TGetDCFunction = function: HDC of object;
   TGLFontNotify = procedure of object;

   TGLFont = class
   private
      FHFont: HFONT;
      FName: WideString;
      FSize: Integer;
      FStyles: TFontStyles;
      FCharSet: Integer;
      FColor: ARGB;
      FColorVector: TColorVector;
      FNotifyChange: TGLFontNotify;

      function FGetWinColor: TColor;
      procedure FSetWinColor(value: TColor);
      procedure FSetColor(value: ARGB);
   public
      constructor Create(Name: WideString; Size: Integer; Styles: TFontStyles = [];
         CharSet: Integer = DEFAULT_CHARSET; Notify: TGLFontNotify = nil);
      destructor Destroy; override;
      procedure Update; // call Update after modifying Name, Size ...

      property HFont: HFONT read FHFont write FHFont;
      property Name: WideString read FName write FName;
      property Size: Integer read FSize write FSize;
      property Styles: TFontStyles read FStyles write FStyles;
      property CharSet: Integer read FCharSet write FCharSet;
      property Color: ARGB read FColor write FSetColor; // do not need to call Update
      property WinColor: TColor read FGetWinColor write FSetWinColor; // do not need to call Update
      property NotifyChange: TGLFontNotify read FNotifyChange write FNotifyChange;
   end;

   TGLCanvas = class
   private
      FHRC: HGLRC;
      FControl: TControl;
      FAfterRendering: TAfterRendering;
      FGetDCFunction: TGetDCFunction;

      FRenderToBmp: Boolean;
      FBufferHDC: HDC;
      FBufferBitmap: HBITMAP;
      FBufferObject: HGDIOBJ;
      FBufferWidth, FBufferHeight: Integer;

      FInvertY: Boolean;
      FRendering: Boolean;
      FBlend: Boolean;
      FAntialiasing: Boolean;
      FIgnorePenWidthFactor: Boolean;

      FListLevel: Integer;
      FIgnoreColor: Boolean;
      FIgnoreColorStack: array[1..ListNestLevel] of Boolean;

      FPenWidthFactor: Single; // Actually the total scale
      FPenWidth: Single;
      FPenColorARGB, FBrushColorARGB: ARGB;
      FPenColor, FBrushColor: TColorVector;

      FMatrix: TMatrix4f; // For backup
      FTransformationUpdateCount: Integer;
      FUseTransformStack: Boolean;
      FTransformStack: array[0..TransformStackTop] of TTransformData;
      FStackTop: Integer;

      FScaleX, FScaleY: Single;
      FTranslateX, FTranslateY: Single;
      FRotation: Single;

      FDefaultFont: TGLFont;
      FASCIICharList: GLuint;
      FASCIICharListCreated: Boolean;

      procedure InitOpenGL;
      procedure ActivateSelf;
      procedure CreateBufferBMP;
      procedure FreeBufferBMP;
      procedure PresentBufferBMP(DC: HDC);

      procedure ApplyTransformation;
      procedure DefaultFontNotify;

      procedure FSetAntialiasing(value: Boolean);
      procedure FSetScaleX(value: Single);
      procedure FSetScaleY(value: Single);
      procedure FSetTranslateX(value: Single);
      procedure FSetTranslateY(value: Single);
      procedure FSetRotation(value: Single);

      procedure FSetPenColor(Value: ARGB);
      procedure FSetBrushColor(Value: ARGB);
      procedure FSetPenWidth(Value: Single);

      procedure EllipseVertices(const x, y, xRadius, yRadius: Single);
   public
      constructor Create(AControl: TControl; RenderToBmp: Boolean = True;
         Antialiasing: Boolean = True; UseTransformStack: Boolean = False;
         IgnorePenWidthFactor: Boolean = False; InvertY: Boolean = True);
      destructor Destroy; override;

      function RenderingBegin(BackgroundColor: TColor = clWhite): TGLCanvas;
      procedure RenderingEnd;

      { You can either build draw lists in Rendering process or not.
        An example:
           GLCanvas.CreateList(List1, 1).ListBegin(List1).Line(0, 0, 50, 50).ListEnd;
           GLCanvas.RenderingBegin.ListExecute(List1).SetTranslateX(50).ListExecute(box).RenderingEnd;
        If IgnoreColor is true, all color definition in fill-shape processes are
        ignored. When the list is built, you can specify color for all vertices of
        the list.  }
      function CreateList(var ListID: GLuint; Range: GLuint = 1): TGLCanvas;
      function DeleteList(ListID: GLuint; Range: GLuint = 1): TGLCanvas;
      function ListBegin(ListID: GLuint; Offset: GLuint = 0; Execute: Boolean = False;
         IgnoreColor: Boolean = True): TGLCanvas;
      function ListEnd: TGLCanvas;
      function ListExecute(ListID: GLuint; Offset: GLuint = 0): TGLCanvas; overload;
      function ListExecute(ListID: GLuint; Offset: GLuint; Color: ARGB;
         PenWidth: Single): TGLCanvas; overload; // PenWidth <= 0 for no change

      { Call Recreate if you want to manually recreate opengl. Normally if Control
        is resized, GLCanvas will detect this change at the beginning of next
        rendering process. }
      procedure Recreate;
      procedure OnControlPaint;
      procedure DrawTo(DC: HDC);
      procedure StretchDrawTo(DC: HDC; X, Y, W, H: Integer);

      ///////////////////////////////////////////////
      function BeginUpdateTransformation: TGLCanvas;
      function EndUpdateTransformation: TGLCanvas;

      // Use transformation for rendering. Not effective for TextOut.
      function SetTransformation(sx, sy, tx, ty, r: Single): TGLCanvas; // Scale, Translate, Rotation
      function SetMatrix(const m11, m12, m13, m21, m22, m23, m31, m32, m33,
         dx, dy: Single; Backup: Boolean = True): TGLCanvas;
      function ResetBackupMatrix: TGLCanvas;

      function ResetTransformation: TGLCanvas;
      function PopMatrix: TGLCanvas; // Cancel last transformation
      function SetEqualScale(value: Single): TGLCanvas;
      function ScaleMatrix(x, y: Single): TGLCanvas;
      function TranslateMatrix(x, y: Single): TGLCanvas;
      function RotateMatrix(angle: Single): TGLCanvas;
      function SetScaleX(value: Single): TGLCanvas;
      function SetScaleY(value: Single): TGLCanvas;
      function SetTranslateX(value: Single): TGLCanvas;
      function SetTranslateY(value: Single): TGLCanvas;
      function SetRotation(value: Single): TGLCanvas;

      function ConvertScreenToWorld(x, y: Integer): TGLPointF; // GDI coordinate where (0, 0) is Left-Top
      function UpdateTransformation: TGLCanvas; // You can maually force update transformation

      ///////////////////////////////////////////////
      function SetBlendState(value: Boolean): TGLCanvas; // Use this to enable or disable GL_BLEND, GL_BLEND is enabled by default.

      ///////////////////////////////////////////////
      function SetPenColorWin(value: TColor; alpha: Byte = 255; Sync: Boolean = True): TGLCanvas;
      function SetBrushColorWin(value: TColor; alpha: Byte = 255; Sync: Boolean = True): TGLCanvas;
      function SetPenColor(value: ARGB): TGLCanvas; overload; // for linked process
      function SetBrushColor(value: ARGB): TGLCanvas; overload; // for linked process
      // For quick OpenGL color assignment. Sync = True, update FPenColorARGB or FBrushColorARGB
      function SetPenColor(const value: TColorVector; Sync: Boolean = False): TGLCanvas; overload;
      function SetBrushColor(const value: TColorVector; Sync: Boolean = False): TGLCanvas; overload;
      function SetPenWidth(value: Single): TGLCanvas; // for linked process

      function LineStipple(factor: Integer; pattern: word): TGLCanvas; overload;
      function LineStipple(style: TLineStippleStyle; enlarge: Byte = 2): TGLCanvas; overload;
      function LineStippleEnd: TGLCanvas;

      function Line(const x1, y1, x2, y2: Integer): TGLCanvas; overload;
      function Line(const x1, y1, x2, y2: Single): TGLCanvas; overload;
      function BeginLines: TGLCanvas; // Remember to call EndLines
      function Lines(const x1, y1, x2, y2: Integer): TGLCanvas; overload;
      function Lines(const x1, y1, x2, y2: Single): TGLCanvas; overload;
      function EndLines: TGLCanvas;
      function Lines(const points: TGLPointsF; count: Integer): TGLCanvas; overload;
      function Lines(const points: TGLPointsI; count: Integer): TGLCanvas; overload;

      function Polyline(const points: TGLPointsF; count: Integer): TGLCanvas; overload;
      function Polyline(const points: TGLPointsI; count: Integer): TGLCanvas; overload;
      function Polygon(const points: TGLPointsF; count: Integer): TGLCanvas; overload;
      function Polygon(const points: TGLPointsI; count: Integer): TGLCanvas; overload;
      function FillPolygon(const points: TGLPointsF; count: Integer; Border: Boolean = False): TGLCanvas; overload;
      function FillPolygon(const points: TGLPointsI; count: Integer; Border: Boolean = False): TGLCanvas; overload;

      function Curve(const points: TGLPointsF; count: Integer; tension: Single = 0.5): TGLCanvas; overload;
      function Curve(const points: TGLPointsI; count: Integer; tension: Single = 0.5): TGLCanvas; overload;
      function ClosedCurve(const points: TGLPointsF; count: Integer; tension: Single = 0.5): TGLCanvas; overload;
      function ClosedCurve(const points: TGLPointsI; count: Integer; tension: Single = 0.5): TGLCanvas; overload;
      function FillClosedCurve(const points: TGLPointsF; count: Integer;
         Border: Boolean = False; tension: Single = 0.5): TGLCanvas; overload;
      function FillClosedCurve(const points: TGLPointsI; count: Integer;
         Border: Boolean = False; tension: Single = 0.5): TGLCanvas; overload;

      function Bezier(const x1, y1, x2, y2, x3, y3, x4, y4: Integer): TGLCanvas; overload;
      function Bezier(const x1, y1, x2, y2, x3, y3, x4, y4: Single): TGLCanvas; overload;
      function PolyBezier(const points: TGLPointsI; count: Integer): TGLCanvas; overload;
      function PolyBezier(const points: TGLPointsF; count: Integer): TGLCanvas; overload;

      // x, y, xRadius, yRadius specify an ellipse. startAngle and sweepAngle specify the range of curve.
      function Arc(const x, y, xRadius, yRadius: Single; startAngle, sweepAngle: Single): TGLCanvas;
      function FillPie(const x, y, xRadius, yRadius: Single;
         startAngle, sweepAngle: Single; Border: Boolean = False): TGLCanvas;

      // Plots a pixel at given coordinate
      function PlotPixel(const x, y: Integer): TGLCanvas; overload;
      function PlotPixel(const x, y: Single): TGLCanvas; overload;
      function BeginPixels: TGLCanvas; // Remember to call EndPixels
      function Pixels(const x, y: Integer): TGLCanvas; overload;
      function Pixels(const x, y: Single): TGLCanvas; overload;
      function EndPixels: TGLCanvas;

      // Draw the (x1,y1)-(x2, y2) rectangle's frame (border). }
      function FrameRect(const x1, y1, x2, y2: Integer): TGLCanvas; overload;
      function FrameRect(const x1, y1, x2, y2: Single): TGLCanvas; overload;
      // Draw the (x1,y1)-(x2, y2) rectangle (filled with BrushColor)
      function FillRect(const x1, y1, x2, y2: Integer; Border: Boolean = False): TGLCanvas; overload;
      function FillRect(const x1, y1, x2, y2: Single; Border: Boolean = False): TGLCanvas; overload;

      function Triangle(const x1, y1, x2, y2, x3, y3: Integer): TGLCanvas; overload;
      function Triangle(const x1, y1, x2, y2, x3, y3: Single): TGLCanvas; overload;
      function FillTriangle(const x1, y1, x2, y2, x3, y3: Integer; Border: Boolean = False): TGLCanvas; overload;
      function FillTriangle(const x1, y1, x2, y2, x3, y3: Single; Border: Boolean = False): TGLCanvas; overload;

      // Draws an ellipse with (x1,y1)-(x2, y2) bounding rectangle.
      function EllipseRect(const x1, y1, x2, y2 : Single): TGLCanvas; overload;
      // Draws and ellipse centered at (x, y) with given radiuses.
      function Ellipse(const x, y, xRadius, yRadius: Single): TGLCanvas; overload;
      function FillEllipseRect(const x1, y1, x2, y2: Single; Border: Boolean = False): TGLCanvas; overload;
      function FillEllipse(const x, y, xRadius, yRadius: Single; Border: Boolean = False): TGLCanvas; overload;

      procedure RecreateDefaultFont;
      // Output only ASCII chars, other chars will be ignored automatically
      function TextOutASCII(const text: string; x, y: Integer; Font: TGLFont = nil): TGLCanvas; // Use this for ASCII chars for efficiency
      // Output any string but slow
      function TextOut(const text: WideString; x, y: Integer; Font: TGLFont = nil): TGLCanvas;

      function BuildTexture(bmp: TBitmap; var texId: GLuint): TGLCanvas;
      function DeleteTexture(texId: GLuint): TGLCanvas;

      //The difference between DrawBitmap and DrawBitmapTex is that DrawBitmapTex supports transformation}
      function DrawBitmap(bmp: TBitmap; x, y: Integer; xZoom: Single = 1.0; yZoom: Single = 1.0): TGLCanvas; overload;
      function DrawBitmapTex(bmp: TBitmap; x, y, w, h: Integer): TGLCanvas; overload;
      function DrawBitmapTex(texId: GLuint; x, y, w, h: Integer): TGLCanvas; overload;

      // Transformation
      property UseTransformStack: Boolean read FUseTransformStack write FUseTransformStack;

      property AfterRendering: TAfterRendering read FAfterRendering write FAfterRendering;
      property GetDCFunction: TGetDCFunction read FGetDCFunction write FGetDCFunction;

      // Turn off UseTransformStack to use these parameters
      property ScaleX: Single read FScaleX write FSetScaleX;
      property ScaleY: Single read FScaleY write FSetScaleY;
      property TranslateX: Single read FTranslateX write FSetTranslateX;
      property TranslateY: Single read FTranslateY write FSetTranslateY;
      property Rotation: Single read FRotation write FSetRotation;

      property RenderToBMP: Boolean read FRenderToBmp;
      property BufferHDC: HDC read FBufferHDC write FBufferHDC;
      property Rendering: Boolean read FRendering;
      property PenColor: ARGB read FPenColorARGB write FSetPenColor;
      property BrushColor: ARGB read FBrushColorARGB write FSetBrushColor;
      property Control: TControl read FControl;

      property InvertY: Boolean read FInvertY;
      property Antialiasing: Boolean read FAntialiasing write FSetAntialiasing;
      property PenWidth: Single read FPenWidth write FSetPenWidth;
      property DefaultFont: TGLFont read FDefaultFont;
   end;

const
   IdentityHmgMatrix: TMatrix4f = ((1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1));

   AlphaShift = 24;
   RedShift = 16;
   GreenShift = 8;
   BlueShift = 0;

function MakeColor(r, g, b: Byte): ARGB; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function MakeColor(a, r, g, b: Byte): ARGB; overload; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function GetAlpha(color: ARGB): BYTE; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function GetRed(color: ARGB): BYTE; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function GetGreen(color: ARGB): BYTE; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function GetBlue(color: ARGB): BYTE; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

// TColor = COLORREF
function TColorToARGB(rgb: TColor; alpha: Byte = 255): ARGB; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function ARGBToTColor(Color: ARGB): TColor; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

function GLColorToARGB(const glcolor: TColorVector): ARGB;  {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function ARGBToGLColor(color: ARGB): TColorVector; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

function GLColorToTColor(const glcolor: TColorVector): TColor; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}
function TColorToGLColor(color: TColor; alpha: Byte = 255): TColorVector; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

function ModifyAlphaValue(col: ARGB; alpha: Byte): ARGB; {$IFDEF INLINE_AVAIL}inline;{$ENDIF}

implementation
var
   GLOBAL_ANTIALIASING_STATE: Boolean;

const
   // Almost all video cards now support these extensions. Even not, no error will be raised.
   GL_TEXTURE_3D = $806F;
   GL_TEXTURE_CUBE_MAP_ARB = $8513;
   GL_BGR = $80E0;

   cNoPrimitive = MaxInt;

   PiDiv180 = Pi / 180;
   _2Pi = Pi * 2;
   PiDiv2 = Pi / 2;

   opengl32 = 'OpenGL32.dll';

procedure glGenTextures(n: GLsizei; textures: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
procedure glBindTexture(target: GLEnum; texture: GLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;
procedure glDeleteTextures(n: GLsizei; textures: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external opengl32;

procedure SinCos(const Theta: Single; var Sin, Cos: Single);
asm
   FLD  Theta
   FSINCOS
   FSTP DWORD PTR [EDX]    // cosine
   FSTP DWORD PTR [EAX]    // sine
end;

function MakeColor(r, g, b: Byte): ARGB; overload;
begin
   with TARGB(Result) do
   begin
      alpha := 255;
      red := r;
      green := g;
      blue := b;
   end;
end;

function MakeColor(a, r, g, b: Byte): ARGB; overload;
begin
   with TARGB(Result) do
   begin
      alpha := a;
      red := r;
      green := g;
      blue := b;
   end;
end;

function GetAlpha(color: ARGB): BYTE;
begin
   Result := BYTE(color shr AlphaShift);
end;

function GetRed(color: ARGB): BYTE;
begin
   Result := BYTE(color shr RedShift);
end;

function GetGreen(color: ARGB): BYTE;
begin
   Result := BYTE(color shr GreenShift);
end;

function GetBlue(color: ARGB): BYTE;
begin
   Result := BYTE(color shr BlueShift);
end;

function TColorToARGB(rgb: TColor; alpha: Byte = 255): ARGB;
begin
   Result := MakeColor(alpha, GetRValue(rgb), GetGValue(rgb), GetBValue(rgb));
end;

function ARGBToTColor(Color: ARGB): TColor;
begin
   with TARGB(Color) do
      Result := RGB(red, green, blue);
end;

function GLColorToARGB(const glcolor: TColorVector): ARGB;
begin
   Result := MakeColor(Trunc(255 * glcolor[3]), Trunc(255 * glcolor[0]),
      Trunc(255 * glcolor[1]), Trunc(255 * glcolor[2]));
end;

function ARGBToGLColor(color: ARGB): TColorVector;
begin
   with TARGB(color) do
   begin
      Result[0] := red / 255;
      Result[1] := green / 255;
      Result[2] := blue / 255;
      Result[3] := alpha / 255;
   end;
end;

function GLColorToTColor(const glcolor: TColorVector): TColor;
begin
   Result := RGB(Trunc(255 * glcolor[0]), Trunc(255 * glcolor[1]), Trunc(255 * glcolor[2]));
end;

function TColorToGLColor(color: TColor; alpha: Byte = 255): TColorVector;
begin
   Result[0] := GetRValue(color) / 255;
   Result[1] := GetGValue(color) / 255;
   Result[2] := GetBValue(color) / 255;
   Result[3] := alpha / 255;
end;

function ModifyAlphaValue(col: ARGB; alpha: Byte): ARGB;
begin
   Result := col;
   TARGB(Result).alpha := alpha;
end;

{ TGLFont }

constructor TGLFont.Create(Name: WideString; Size: Integer; Styles: TFontStyles = [];
   CharSet: Integer = DEFAULT_CHARSET; Notify: TGLFontNotify = nil);
begin
   FHFont := 0;
   FName := Name;
   FSize := Size;
   FStyles := Styles;
   FCharSet := CharSet;
   FColor := TColorToARGB(clBlack);
   FNotifyChange := Notify;
   Update;
end;

destructor TGLFont.Destroy;
begin
   DeleteObject(FHFont);
   inherited;
end;

function TGLFont.FGetWinColor: TColor;
begin
   Result := ARGBToTColor(FColor);
end;

procedure TGLFont.FSetWinColor(value: TColor);
begin
   Color := TColorToARGB(value);
end;

procedure TGLFont.FSetColor(value: ARGB);
begin
   FColor := value;
   FColorVector := ARGBToGLColor(FColor);
end;

procedure TGLFont.Update;
var
   bold, italic, underline, strikeout: Integer;
begin
   if FHFont <> 0 then
      DeleteObject(FHFont);

   if fsBold in FStyles then
      bold := FW_BOLD
   else
      bold := FW_NORMAL;

   if fsItalic in FStyles then
      italic := 1
   else
      italic := 0;

   if fsUnderline in FStyles then
      underline := 1
   else
      underline := 0;

   if fsStrikeOut in FStyles then
      strikeout := 1
   else
      strikeout := 0;

   FHFont := CreateFontW(FSize, 0, 0, 0, bold, italic, underline, strikeout,
        FCharSet, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
        DEFAULT_QUALITY, DEFAULT_PITCH or FF_SWISS, PWideChar(FName));

   FColorVector := ARGBToGLColor(FColor);

   if Assigned(FNotifyChange) then
      FNotifyChange;
end;

{ TGLCanvas }

constructor TGLCanvas.Create(AControl: TControl; RenderToBmp: Boolean = True;
   Antialiasing: Boolean = True; UseTransformStack: Boolean = False;
   IgnorePenWidthFactor: Boolean = False; InvertY: Boolean = True);
begin
   FControl := AControl;
   FRenderToBmp := RenderToBmp;
   if not FRenderToBmp then // Require that FControl is TWinControl
      if not (FControl is TWinControl) then
         FRenderToBmp := True;

   FUseTransformStack := UseTransformStack;
   FInvertY := InvertY;
   FRendering := False;
   FAntialiasing := Antialiasing;
   FIgnorePenWidthFactor := IgnorePenWidthFactor;
   FListLevel := 0;
   FIgnoreColor := False;
   FPenWidth := 1.0;
   FStackTop := -1;
   FTranslateX := 0.0;
   FTranslateY := 0.0;
   FScaleX := 1.0;
   FScaleY := 1.0;
   FRotation := 0.0;
   FTransformationUpdateCount := 0;
   SetPenColorWin(clBlack);
   SetBrushColorWin(clRed);

   FASCIICharListCreated := False;
   FDefaultFont := TGLFont.Create('Tahoma', 14);
   FDefaultFont.NotifyChange := DefaultFontNotify; // Mustn't assign NotifyChange by TGLFont.Create

   FBufferWidth := -1;
   Recreate;
end;

destructor TGLCanvas.Destroy;
begin
   FreeBufferBMP;
   wglDeleteContext(FHRC);
   FDefaultFont.Free;
   inherited;
end;

procedure TGLCanvas.CreateBufferBMP;
var
   bmi: BITMAPINFO;
   pbits: ^DWORD;
begin
   if FBufferWidth <> -1 then
      FreeBufferBMP;

   FBufferWidth := FControl.ClientWidth;
   FBufferHeight := FControl.ClientHeight;
   // Create a memory DC compatible with the screen
   FBufferHDC := CreateCompatibleDC(0);

   FillChar(bmi, SizeOf(bmi), 0);
   with bmi.bmiHeader do
   begin
      biSize := SizeOf(BITMAPINFOHEADER);
      biWidth := FBufferWidth;
      biHeight := FBufferHeight;
      biPlanes := 1;
      biBitCount := 32;
      biCompression := BI_RGB;
   end;

   FBufferBitmap := CreateDIBSection(FBufferHDC, bmi, DIB_RGB_COLORS, Pointer(pbits), 0, 0);

   // Select the bitmap into the DC
   FBufferObject := SelectObject(FBufferHDC, FBufferBitmap);
end;

procedure TGLCanvas.FreeBufferBMP;
begin
   SelectObject(FBufferHDC, FBufferObject); // Remove bitmap from DC
   DeleteObject(FBufferBitmap); // Delete bitmap
   DeleteDC(FBufferHDC); // Delete DC
end;

procedure TGLCanvas.PresentBufferBMP(DC: HDC);
begin
   StretchBlt(DC, 0, 0, FControl.ClientWidth,
      FControl.ClientHeight, FBufferHDC, 0, 0, FBufferWidth,
      FBufferHeight, SRCCOPY);
end;

procedure TGLCanvas.FSetPenColor(Value: ARGB);
begin
   if FPenColorARGB <> Value then
   begin
      FPenColorARGB := Value;
      FPenColor := ARGBToGLColor(Value);
   end;
   glColor4fv(@FPenColor);
end;

procedure TGLCanvas.FSetBrushColor(Value: ARGB);
begin
   if FBrushColorARGB <> Value then
   begin
      FBrushColorARGB := Value;
      FBrushColor := ARGBToGLColor(Value);
   end;
end;

procedure TGLCanvas.FSetPenWidth(Value: Single);
begin
   FPenWidth := Value;
   glLineWidth(Value * FPenWidthFactor);
   glPointSize(Value * FPenWidthFactor);
end;

procedure TGLCanvas.InitOpenGL;
var
   pfd: TPIXELFORMATDESCRIPTOR;
   pixelFormat: Integer;
begin
   FillChar(pfd, SizeOf(pfd), 0);
   with pfd do
   begin
      nSize := SizeOf(TPIXELFORMATDESCRIPTOR); // 此结构尺寸
      nVersion := 1;
      if FRenderToBmp then
         dwFlags := PFD_SUPPORT_OPENGL or PFD_DRAW_TO_BITMAP
      else
         dwFlags := PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER or PFD_DRAW_TO_WINDOW;
      iPixelType := PFD_TYPE_RGBA; //使用RGBA颜色空间
      cColorBits := 32;
      cDepthBits := 16;
      iLayerType := PFD_MAIN_PLANE;
   end;

   pixelFormat := ChoosePixelFormat(FBufferHDC, @pfd);
   SetPixelFormat(FBufferHDC, pixelFormat, @pfd);
   FHRC := wglCreateContext(FBufferHDC);
   wglMakeCurrent(FBufferHDC, FHRC);
   GdiFlush();

   glPushAttrib(GL_ENABLE_BIT);
   glDisable(GL_CULL_FACE);
   glDisable(GL_LIGHTING);
   glDisable(GL_FOG);
   glDisable(GL_COLOR_MATERIAL);
   glDisable(GL_DEPTH_TEST);
   glDisable(GL_TEXTURE_1D);
   glDisable(GL_TEXTURE_2D);
   glDisable(GL_TEXTURE_3D);
   glDisable(GL_TEXTURE_CUBE_MAP_ARB);

   FBlend := True;
   glEnable(GL_BLEND);
   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

   Antialiasing := FAntialiasing; // Apply antialiasing mode

   glViewPort(0, 0, FBufferWidth, FBufferHeight); //指定OpenGL在此区域内绘图。
   glMatrixMode(GL_PROJECTION);
   if FInvertY then
      gluOrtho2D(0, FBufferWidth, 0, FBufferHeight)
   else
      gluOrtho2D(0, FBufferWidth, FBufferHeight, 0);
   glMatrixMode(GL_MODELVIEW);

   // Recreate ASCII char lists
   DefaultFontNotify;
end;

procedure TGLCanvas.ActivateSelf;
begin
   wglMakeCurrent(FBufferHDC, FHRC);
   if GLOBAL_ANTIALIASING_STATE <> FAntialiasing then
      FSetAntialiasing(FAntialiasing);
   if FBlend then
      glEnable(GL_BLEND)
   else
      glDisable(GL_BLEND);
end;

procedure TGLCanvas.ApplyTransformation;
   function ComputeCompoundPenWidthFactor(sx, sy: Single): Single;
   begin
      Result := Sqrt((sx * sx + sy * sy) / 2);
   end;
var
   i: Integer;
   tx, ty: Single;
begin
   if not FRendering then
      Exit;
   if FUseTransformStack then
   begin
      glLoadIdentity;
      tx := 1.0;
      ty := 1.0;
      for i := 0 to FStackTop do
         with FTransformStack[i] do
         begin
            case TransformType of
               ttScale:
                  begin
                     glScale(var1, var2, 1.0);
                     tx := tx * var1;
                     ty := ty * var2;
                  end;
               ttTranslate: glTranslate(var1, var2, 0.0);
               ttRotate: glRotatef(var1, 0.0, 0.0, 1.0);
            end;
         end;
      if FIgnorePenWidthFactor then
         FPenWidthFactor := 1
      else
         FPenWidthFactor := ComputeCompoundPenWidthFactor(tx, ty);
   end
   else
   begin
      glLoadIdentity;
      glTranslate(FTranslateX, FTranslateY, 0.0);
      glScale(FScaleX, FScaleY, 1.0);
      glRotatef(FRotation, 0.0, 0.0, 1.0);
      if FIgnorePenWidthFactor then
         FPenWidthFactor := 1
      else
         FPenWidthFactor := ComputeCompoundPenWidthFactor(FScaleX, FScaleY);
   end;
end;

procedure TGLCanvas.DefaultFontNotify;
var
   i: Integer;
begin
   for i := 0 to 1 do // very odd, must call twice
   begin
      if FASCIICharListCreated then
         glDeleteLists(FASCIICharList, MaxChar);
      SelectObject(FBufferHDC, FDefaultFont.HFont);
      FASCIICharList := glGenLists(MaxChar);
      wglUseFontBitmaps(FBufferHDC, 0, MaxChar, FASCIICharList);
      FASCIICharListCreated := True;
   end;
end;

procedure TGLCanvas.FSetAntialiasing(value: Boolean);
begin
   if value then
   begin
      glEnable(GL_SMOOTH);
      glEnable(GL_LINE_SMOOTH);
      glEnable(GL_POINT_SMOOTH);
      glEnable(GL_POLYGON_SMOOTH);
      glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);
      glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
      glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
   end
   else
   begin
      glDisable(GL_SMOOTH);
      glDisable(GL_LINE_SMOOTH);
      glDisable(GL_POINT_SMOOTH);
      glDisable(GL_POLYGON_SMOOTH);
      glHint(GL_POINT_SMOOTH_HINT, GL_FASTEST);
      glHint(GL_LINE_SMOOTH_HINT, GL_FASTEST);
      glHint(GL_POLYGON_SMOOTH_HINT, GL_FASTEST);
   end;
   FAntialiasing := value;
   GLOBAL_ANTIALIASING_STATE := FAntialiasing;
end;

procedure TGLCanvas.FSetScaleX(value: Single);
begin
   FScaleX := value;
   if FTransformationUpdateCount = 0 then
      ApplyTransformation;
end;

procedure TGLCanvas.FSetScaleY(value: Single);
begin
   FScaleY := value;
   if FTransformationUpdateCount = 0 then
      ApplyTransformation;
end;

procedure TGLCanvas.FSetTranslateX(value: Single);
begin
   FTranslateX := value;
   if FTransformationUpdateCount = 0 then
      ApplyTransformation;
end;

procedure TGLCanvas.FSetTranslateY(value: Single);
begin
   FTranslateY := value;
   if FTransformationUpdateCount = 0 then
      ApplyTransformation;
end;

procedure TGLCanvas.FSetRotation(value: Single);
begin
   FRotation := value;
   if FTransformationUpdateCount = 0 then
      ApplyTransformation;
end;

function TGLCanvas.RenderingBegin(BackgroundColor: TColor = clWhite): TGLCanvas;
var
   bkCol: TColorVector;
begin
   Result := Self;
   if FRendering then
      Exit;
   FRendering := True;
   if (FBufferWidth <> FControl.ClientWidth) or (FBufferHeight <> FControl.ClientHeight) then
      Recreate
   {$IFDEF MultiCanvases}
   else
      ActivateSelf{$ENDIF}; // get active

   bkCol := TColorToGLColor(BackgroundColor);
   glClearColor(bkCol[0], bkCol[1], bkCol[2], bkCol[3]);
   glClear(GL_COLOR_BUFFER_BIT);

   ApplyTransformation;

   // User must keep in mind that Matrix Mode mustn't be changed during rendering.
   glColor4fv(@FPenColor);
end;

procedure TGLCanvas.RenderingEnd;
begin
   if FRenderToBMP then
   begin
      glFinish;
      // User may draw other objects using Gdi, GdiP after OpenGL process.
      if Assigned(FAfterRendering) then
         FAfterRendering(Self);
      // Present the image
      if FControl is TImage then // TImage has the ability to keep its image
         PresentBufferBMP(TImage(FControl).Canvas.Handle);
      FControl.Repaint; // Other controls must repaint whenever they are repainted by Windows.
   end
   else
      SwapBuffers(FBufferHDC);

   FRendering := False;
end;

function TGLCanvas.CreateList(var ListID: GLuint; Range: GLuint = 1): TGLCanvas;
begin
   if Range >= 1 then
   begin
      {$IFDEF MultiCanvases}
      ActivateSelf;
      {$ENDIF}
      ListID := glGenLists(Range);
   end;
   Result := Self;
end;

function TGLCanvas.DeleteList(ListID: GLuint; Range: GLuint = 1): TGLCanvas;
begin
   glDeleteLists(ListID, Range);
   Result := Self;
end;

function TGLCanvas.ListBegin(ListID: GLuint; Offset: GLuint = 0;
   Execute: Boolean = False; IgnoreColor: Boolean = True): TGLCanvas;
begin
   Result := Self;
   if FListLevel = ListNestLevel then
      Exit;
      
   if Execute then
      glNewList(ListID + Offset, GL_COMPILE_AND_EXECUTE)
   else
      glNewList(ListID + Offset, GL_COMPILE);
   Inc(FListLevel);
   FIgnoreColorStack[FListLevel] := IgnoreColor;       
   FIgnoreColor := IgnoreColor;
end;

function TGLCanvas.ListEnd: TGLCanvas;
begin
   glEndList;
   Dec(FListLevel);
   if FListLevel > 0 then
      FIgnoreColor := FIgnoreColorStack[FListLevel]
   else  
      FIgnoreColor := False;    
   Result := Self;      
end;

function TGLCanvas.ListExecute(ListID: GLuint; Offset: GLuint = 0): TGLCanvas;
begin
   if FRendering then
      glCallList(ListID + Offset);
   Result := Self;
end;

function TGLCanvas.ListExecute(ListID: GLuint; Offset: GLuint; Color: ARGB;
   PenWidth: Single): TGLCanvas;
var
   ColorVec: TColorVector;
begin
   if FRendering then
   begin
      ColorVec := ARGBToGLColor(Color);
      glColor4fv(@ColorVec);
      if PenWidth > 0 then
      begin
         glLineWidth(PenWidth * FPenWidthFactor);
         glPointSize(PenWidth * FPenWidthFactor);
      end;
      glCallList(ListID + Offset);

      // Restore color and pen width
      glColor4fv(@FPenColor);
      if PenWidth > 0 then
      begin
         glLineWidth(FPenWidth * FPenWidthFactor);
         glPointSize(FPenWidth * FPenWidthFactor);
      end;
   end;
   Result := Self;
end;

procedure TGLCanvas.Recreate;
begin
   // Reinitialize opengl
   if FRenderToBmp then
      CreateBufferBMP
   else
   begin
      FBufferHDC := GetDC(TWinControl(FControl).Handle);
      FBufferWidth := FControl.ClientWidth;
      FBufferHeight := FControl.ClientHeight;
   end;
   wglDeleteContext(FHRC);
   InitOpenGL;
end;

procedure TGLCanvas.OnControlPaint;
var
   DC: HDC;
begin
   if FRenderToBmp then
   begin
      if FControl is TWinControl then
         DC := GetDC(TWinControl(FControl).Handle)
      else if Assigned(FGetDCFunction) then
         DC := FGetDCFunction()
      else
         DC := 0;

      if DC <> 0 then
         PresentBufferBMP(DC);
   end;
end;

procedure TGLCanvas.DrawTo(DC: HDC);
begin
   if FRenderToBmp then
      StretchBlt(DC, 0, 0, FBufferWidth, FBufferHeight,
         FBufferHDC, 0, 0, FBufferWidth, FBufferHeight, SRCCOPY);
end;

procedure TGLCanvas.StretchDrawTo(DC: HDC; X, Y, W, H: Integer);
begin
   if FRenderToBmp then
      StretchBlt(DC, X, Y, W, H, FBufferHDC, 0, 0, FBufferWidth, FBufferHeight, SRCCOPY);
end;

function TGLCanvas.BeginUpdateTransformation: TGLCanvas;
begin
   Inc(FTransformationUpdateCount);
   Result := Self;
end;

function TGLCanvas.EndUpdateTransformation: TGLCanvas;
begin
   Dec(FTransformationUpdateCount);
   if FTransformationUpdateCount <= 0 then
   begin
      UpdateTransformation;
      FTransformationUpdateCount := 0;
   end;
   Result := Self;
end;

function TGLCanvas.SetTransformation(sx, sy, tx, ty, r: Single): TGLCanvas;
begin
   FUseTransformStack := False;
   FScaleX := sx;
   FScaleY := sy;
   FTranslateX := tx;
   FTranslateY := ty;
   FRotation := r;
   if FTransformationUpdateCount = 0 then
      ApplyTransformation;
   Result := Self;
end;

function TGLCanvas.SetMatrix(const m11, m12, m13, m21, m22, m23, m31, m32, m33,
   dx, dy: Single; Backup: Boolean = True): TGLCanvas;
var
   AMatrix: TMatrix4f;
begin
   Result := Self;
   if not Rendering then
      Exit;

   AMatrix := IdentityHmgMatrix;
   AMatrix[0][0] := m11;
   AMatrix[0][1] := m12;
   AMatrix[0][2] := m13;
   AMatrix[1][0] := m21;
   AMatrix[1][1] := m22;
   AMatrix[1][2] := m23;
   AMatrix[2][0] := m31;
   AMatrix[2][1] := m32;
   AMatrix[2][2] := m33;
   AMatrix[3][0] := dx;
   AMatrix[3][1] := dy;
   glLoadMatrixf(@AMatrix);

   if Backup then
      FMatrix := AMatrix;

   // Assume that X axis and Y axis are equally scaled.
   if FIgnorePenWidthFactor then
      FPenWidthFactor := 1
   else
      FPenWidthFactor := Sqrt((m11 * m11 + m22 * m22) / 2);
end;

function TGLCanvas.ResetBackupMatrix: TGLCanvas;
begin
   Result := Self;
   if not Rendering then
      Exit;

   glLoadMatrixf(@FMatrix);
   // Assume that X axis and Y axis are equally scaled.
   if FIgnorePenWidthFactor then
      FPenWidthFactor := 1
   else
      FPenWidthFactor := Sqrt((Sqr(FMatrix[0][0]) + Sqr(FMatrix[1][1])) / 2);
end;

function TGLCanvas.ResetTransformation: TGLCanvas;
begin
   FStackTop := -1;
   FScaleX := 1.0;
   FScaleY := 1.0;
   FTranslateX := 0.0;
   FTranslateY := 0.0;
   FRotation := 0.0;
   if FTransformationUpdateCount = 0 then
      ApplyTransformation;
   Result := Self;
end;

function TGLCanvas.PopMatrix: TGLCanvas;
begin
   if FStackTop >= 0 then
      Dec(FStackTop);
   if FTransformationUpdateCount = 0 then
      ApplyTransformation;
   Result := Self;
end;

function TGLCanvas.SetEqualScale(value: Single): TGLCanvas;
begin
   if not FUseTransformStack then
   begin
      FScaleX := value;
      FScaleY := value;
   end;
   if FTransformationUpdateCount = 0 then
      ApplyTransformation;
   Result := Self;
end;

function TGLCanvas.ScaleMatrix(x, y: Single): TGLCanvas;
begin
   if FStackTop < TransformStackTop then
   begin
      Inc(FStackTop);
      with FTransformStack[FStackTop] do
      begin
         TransformType := ttScale;
         var1 := x;
         var2 := y;
      end;
   end;
   if FTransformationUpdateCount = 0 then
      ApplyTransformation;
   Result := Self;
end;

function TGLCanvas.TranslateMatrix(x, y: Single): TGLCanvas;
begin
   if FStackTop < TransformStackTop then
   begin
      Inc(FStackTop);
      with FTransformStack[FStackTop] do
      begin
         TransformType := ttTranslate;
         var1 := x;
         var2 := y;
      end;
   end;
   if FTransformationUpdateCount = 0 then
      ApplyTransformation;
   Result := Self;
end;

function TGLCanvas.RotateMatrix(angle: Single): TGLCanvas;
begin
   if FStackTop < TransformStackTop then
   begin
      Inc(FStackTop);
      with FTransformStack[FStackTop] do
      begin
         TransformType := ttRotate;
         var1 := angle;
      end;
   end;
   if FTransformationUpdateCount = 0 then
      ApplyTransformation;
   Result := Self;
end;

function TGLCanvas.SetScaleX(value: Single): TGLCanvas;
begin
   ScaleX := value;
   Result := Self;
end;

function TGLCanvas.SetScaleY(value: Single): TGLCanvas;
begin
   ScaleY := value;
   Result := Self;
end;

function TGLCanvas.SetTranslateX(value: Single): TGLCanvas;
begin
   TranslateX := value;
   Result := Self;
end;

function TGLCanvas.SetTranslateY(value: Single): TGLCanvas;
begin
   TranslateY := value;
   Result := Self;
end;

function TGLCanvas.SetRotation(value: Single): TGLCanvas;
begin
   Rotation := value;
   Result := Self;
end;

function TGLCanvas.ConvertScreenToWorld(x, y: Integer): TGLPointF;
var
   Viewport: array[0..3] of GLuint;
   ModelMatrix: array[0..15] of GLdouble;
   ProjMatrix: array[0..15] of GLdouble;
   ox, oy, oz: GLdouble;
begin
   glGetIntegerv(GL_VIEWPORT, @Viewport[0]);
   glGetDoublev(GL_MODELVIEW_MATRIX, @ModelMatrix[0]);
   glGetDoublev(GL_PROJECTION_MATRIX, @ProjMatrix[0]);
   gluUnProject(x, Viewport[3] - y, 0, @ModelMatrix[0],
      @ProjMatrix[0], @Viewport[0], ox, oy, oz);
   Result.X := ox;
   Result.Y := oy;
end;

function TGLCanvas.UpdateTransformation: TGLCanvas;
begin
   ApplyTransformation;
   PenWidth := FPenWidth;
   Result := Self;
end;

function TGLCanvas.SetBlendState(value: Boolean): TGLCanvas;
begin
   if value then
      glEnable(GL_BLEND)
   else
      glDisable(GL_BLEND);
   FBlend := value;
   Result := Self;
end;

function TGLCanvas.SetPenColorWin(value: TColor; alpha: Byte = 255; Sync: Boolean = True): TGLCanvas;
begin
   if Sync then
      FPenColorARGB := TColorToARGB(value);
   FPenColor := TColorToGLColor(value, alpha);
   glColor4fv(@FPenColor);
   Result := Self;
end;

function TGLCanvas.SetBrushColorWin(value: TColor; alpha: Byte = 255; Sync: Boolean = True): TGLCanvas;
begin
   if Sync then
      FBrushColorARGB := TColorToARGB(value);
   FBrushColor := TColorToGLColor(value, alpha);
   Result := Self;
end;

function TGLCanvas.SetPenColor(value: ARGB): TGLCanvas;
begin
   FSetPenColor(value);
   Result := Self;
end;

function TGLCanvas.SetBrushColor(value: ARGB): TGLCanvas;
begin
   FSetBrushColor(value);
   Result := Self;
end;

function TGLCanvas.SetPenColor(const value: TColorVector; Sync: Boolean = False): TGLCanvas;
begin
   glColor4fv(@value);
   FPenColor := value;
   if Sync then
      FPenColorARGB := GLColorToARGB(value);
   Result := Self;
end;

function TGLCanvas.SetBrushColor(const value: TColorVector; Sync: Boolean = False): TGLCanvas;
begin
   FBrushColor := value;
   if Sync then
      FBrushColorARGB := GLColorToARGB(value);
   Result := Self;
end;

function TGLCanvas.SetPenWidth(value: Single): TGLCanvas; // for linked process
begin
   FSetPenWidth(value);
   Result := Self;
end;

function TGLCanvas.LineStipple(factor: Integer; pattern: word): TGLCanvas;
begin
   glEnable(GL_LINE_STIPPLE);
   glLineStipple(factor, pattern);
   Result := Self;
end;

function TGLCanvas.LineStipple(style: TLineStippleStyle; enlarge: Byte = 2): TGLCanvas;
begin
   case style of
      lssSolid: LineStippleEnd;
      lssDash: LineStipple(3 * enlarge, $AAAA);
      lssDashDot: LineStipple(1 * enlarge, $6F6F);
      lssDashDotDot: LineStipple(2 * enlarge, $EAEA);
      lssDot: LineStipple(1 * enlarge, $AAAA);
   end;
   Result := Self;
end;

function TGLCanvas.LineStippleEnd: TGLCanvas;
begin
   glDisable(GL_LINE_STIPPLE);
   Result := Self;
end;

function TGLCanvas.Line(const x1, y1, x2, y2: Integer): TGLCanvas;
begin
   glBegin(GL_LINES);
   glVertex2i(x1, y1);
   glVertex2i(x2, y2);
   glEnd;
   Result := Self;
end;

function TGLCanvas.Line(const x1, y1, x2, y2: Single): TGLCanvas;
begin
   glBegin(GL_LINES);
   glVertex2f(x1, y1);
   glVertex2f(x2, y2);
   glEnd;
   Result := Self;
end;

function TGLCanvas.BeginLines: TGLCanvas;
begin
   glBegin(GL_LINES);
   Result := Self;
end;

function TGLCanvas.Lines(const x1, y1, x2, y2: Integer): TGLCanvas;
begin
   glVertex2i(x1, y1);
   glVertex2i(x2, y2);
   Result := Self;
end;

function TGLCanvas.Lines(const x1, y1, x2, y2: Single): TGLCanvas;
begin
   glVertex2f(x1, y1);
   glVertex2f(x2, y2);
   Result := Self;
end;

function TGLCanvas.EndLines: TGLCanvas;
begin
   glEnd;
   Result := Self;
end;

function TGLCanvas.Lines(const points: TGLPointsF; count: Integer): TGLCanvas;
var
   i: Integer;
begin
   glBegin(GL_LINES);
   i := 0;
   while (i <= count - 2) do
   begin
      glVertex2f(points[i].X, points[i].Y);
      glVertex2f(points[i + 1].X, points[i + 1].Y);
      Inc(i, 2);
   end;
   glEnd;
   Result := Self;
end;

function TGLCanvas.Lines(const points: TGLPointsI; count: Integer): TGLCanvas;
var
   i: Integer;
begin
   glBegin(GL_LINES);
   i := 0;
   while (i <= count - 2) do
   begin
      glVertex2i(points[i].X, points[i].Y);
      glVertex2i(points[i + 1].X, points[i + 1].Y);
      Inc(i, 2);
   end;
   glEnd;
   Result := Self;
end;

function TGLCanvas.Polyline(const points: TGLPointsF; count: Integer): TGLCanvas;
var
   i: Integer;
begin
   if count > 1 then
   begin
      glBegin(GL_LINE_STRIP);
      for i := 0 to count - 1 do
         glVertex2f(points[i].X, points[i].Y);
      glEnd;
   end;
   Result := Self;
end;

function TGLCanvas.Polygon(const points: TGLPointsF; count: Integer): TGLCanvas;
var
   i: Integer;
begin
   if count > 1 then
   begin
      glBegin(GL_LINE_LOOP);
      for i := 0 to count - 1 do
         glVertex2f(points[i].X, points[i].Y);
      glEnd;
   end;
   Result := Self;
end;

function TGLCanvas.Polyline(const points: TGLPointsI; count: Integer): TGLCanvas;
var
   i: Integer;
begin
   if count > 1 then
   begin
      glBegin(GL_LINE_STRIP);
      for i := 0 to count - 1 do
         glVertex2i(points[i].X, points[i].Y);
      glEnd;
   end;
   Result := Self;
end;

function TGLCanvas.Polygon(const points: TGLPointsI; count: Integer): TGLCanvas;
var
   i: Integer;
begin
   if count > 1 then
   begin
      glBegin(GL_LINE_LOOP);
      for i := 0 to count - 1 do
         glVertex2i(points[i].X, points[i].Y);
      glEnd;
   end;
   Result := Self;
end;

function TGLCanvas.FillPolygon(const points: TGLPointsF; count: Integer;
   Border: Boolean = False): TGLCanvas;
var
   i: Integer;
begin
   if count > 1 then
   begin
      if not FIgnoreColor then
         glColor4fv(@FBrushColor);
      glBegin(GL_POLYGON);
      for i := 0 to count - 1 do
         glVertex2f(points[i].X, points[i].Y);
      glEnd;
      if not FIgnoreColor then 
         glColor4fv(@FPenColor);

      if Border then
         Polygon(points, count);
   end;
   Result := Self;
end;

function TGLCanvas.FillPolygon(const points: TGLPointsI; count: Integer;
   Border: Boolean = False): TGLCanvas;
var
   i: Integer;
begin
   if count > 1 then
   begin
      if not FIgnoreColor then 
         glColor4fv(@FBrushColor);
      glBegin(GL_POLYGON);
      for i := 0 to count - 1 do
         glVertex2i(points[i].X, points[i].Y);
      glEnd;   
      if not FIgnoreColor then 
         glColor4fv(@FPenColor);

      if Border then
         Polygon(points, count);
   end;
   Result := Self;
end;

{ The following methods are translated into Pascal from ReactOS source.
    calc_curve_bezier_endp
    calc_curve_bezier
    BEZIERMIDDLE
    BezierCheck
    GDI_InternalBezier
    GDI_Bezier
    GenCurvePoints  }
// Calculates Bezier points from cardinal spline endpoints.
procedure calc_curve_bezier_endp(xend, yend, xadj, yadj, tension: Single;
   var x, y: Single);
begin
   // tangent at endpoints is the line from the endpoint to the adjacent point
   x := tension * (xadj - xend) + xend;
   y := tension * (yadj - yend) + yend;
end;

// Calculates Bezier points from cardinal spline points.
procedure calc_curve_bezier(const pts: TGLPointsF; tension: Single;
   var x1, y1, x2, y2: Single);
var
   xdiff, ydiff: Single;
begin
   // calculate tangent
   xdiff := pts[2].X - pts[0].X;
   ydiff := pts[2].Y - pts[0].Y;

   // apply tangent to get control points
   x1 := pts[1].X - tension * xdiff;
   y1 := pts[1].Y - tension * ydiff;
   x2 := pts[1].X + tension * xdiff;
   y2 := pts[1].Y + tension * ydiff;
end;

procedure BEZIERMIDDLE(var Mid: TGLPointF; const P1, P2: TGLPointF);
begin
   Mid.x := (P1.x + P2.x) / 2;
   Mid.y := (P1.y + P2.y) / 2;
end;

type
   TGDIBezierPoints = array[0..3] of TGLPointF;

{
* BezierCheck helper function to check
* that recursion can be terminated
*       Points[0] and Points[3] are begin and endpoint
*       Points[1] and Points[2] are control points
*       level is the recursion depth
*       returns true if the recusion can be terminated
}
function BezierCheck(level: Integer; const Points: TGDIBezierPoints): Boolean;
const
   BEZIERPIXEL = 1;
var
   dx, dy: Single;
begin
   dx := Points[3].x - Points[0].x;
   dy := Points[3].y - Points[0].y;
   if Abs(dy) <= Abs(dx) then // shallow line
   begin
      // check that control points are between begin and end
      if Points[1].x < Points[0].x then
      begin
         if Points[1].x < Points[3].x then
         begin
            Result := False;
            Exit;
         end;
      end
      else if Points[1].x > Points[3].x then
      begin
         Result := False;
         Exit;
      end;

      if Points[2].x < Points[0].x then
      begin
         if Points[2].x < Points[3].x then
         begin
            Result := False;
            Exit;
         end;
      end
      else if Points[2].x > Points[3].x then
      begin
         Result := False;
         Exit;
      end;

      if IsZero(dx) then
      begin
         Result := True;
         Exit;
      end;

      if (Abs(Points[1].y - Points[0].y - (dy / dx) * (Points[1].x - Points[0].x)) > BEZIERPIXEL) or
         (Abs(Points[2].y - Points[0].y - (dy / dx) * (Points[2].x - Points[0].x)) > BEZIERPIXEL) then
      begin
         Result := False;
         Exit;
      end
      else
      begin
         Result := True;
         Exit;
      end;
   end
   else
   begin // steep line
      // check that control points are between begin and end
      if Points[1].y < Points[0].y then
      begin
         if Points[1].y < Points[3].y then
         begin
            Result := False;
            Exit;
         end;
      end
      else if Points[1].y > Points[3].y then
      begin
         Result := False;
         Exit;
      end;

      if Points[2].y < Points[0].y then
      begin
         if Points[2].y < Points[3].y then
         begin
            Result := False;
            Exit;
         end;
      end
      else if Points[2].y > Points[3].y then
      begin
         Result := False;
         Exit;
      end;

      if IsZero(dy) then
      begin
         Result := True;
         Exit;
      end;

      if (Abs(Points[1].x - Points[0].x - (dx / dy) * (Points[1].y - Points[0].y)) > BEZIERPIXEL) or
        (Abs(Points[2].x - Points[0].x - (dx / dy) * (Points[2].y - Points[0].y)) > BEZIERPIXEL) then
      begin
         Result := False;
         Exit;
      end
      else
      begin
         Result := True;
         Exit;
      end;
   end;
end;

procedure GDI_InternalBezier(var Points: TGDIBezierPoints; var PtsOut: TGLPointsF;
   var dwOut, nPtsOut: Integer; level: Integer);
var
   Points2: TGDIBezierPoints; // for the second recursive call
begin
  if nPtsOut = dwOut then
  begin
     dwOut := dwOut * 2;
     SetLength(PtsOut, dwOut);
  end;

  if (level = 0) or BezierCheck(level, Points) then // Recursion can be terminated
  begin
     if nPtsOut = 0 then
     begin
        PtsOut[0] := Points[0];
        nPtsOut := 1;
     end;
     PtsOut[nPtsOut] := Points[3];
     Inc(nPtsOut);
  end
  else
  begin
     Points2[3] := Points[3];
     BEZIERMIDDLE(Points2[2], Points[2], Points[3]);
     BEZIERMIDDLE(Points2[0], Points[1], Points[2]);
     BEZIERMIDDLE(Points2[1],Points2[0],Points2[2]);

     BEZIERMIDDLE(Points[1], Points[0],  Points[1]);
     BEZIERMIDDLE(Points[2], Points[1], Points2[0]);
     BEZIERMIDDLE(Points[3], Points[2], Points2[1]);

     Points2[0] := Points[3];

     // do the two halves
     GDI_InternalBezier(Points, PtsOut, dwOut, nPtsOut, level - 1);
     GDI_InternalBezier(Points2, PtsOut, dwOut, nPtsOut, level - 1);
  end;
end;

procedure GDI_Bezier(const Points: TGLPointsF; count: Integer;
   var PtsOut: TGLPointsF; var nPtsOut:Integer);
var
   Bezier, dwOut: Integer;
   ptBuf: TGDIBezierPoints;
begin
   dwOut := 150;
   nPtsOut := 0;

   if (count - 1) mod 3 <> 0 then
      Exit;

   SetLength(PtsOut, dwOut);
   for Bezier := 0 to (count - 1) div 3 - 1 do
   begin
      Move(Points[Bezier * 3], ptBuf[0], SizeOf(ptBuf));
      GDI_InternalBezier(ptBuf, PtsOut, dwOut, nPtsOut, 8);
   end;
end;

procedure GenCurvePoints(const points: TGLPointsF; count: Integer;
   var outPoints: TGLPointsF; var outCount: Integer; tension: Single = 0.5);
var
   i, len_pt: Integer;
   x1, x2, y1, y2: Single;
   pt: TGLPointsF;
begin
   outCount := 0;
   if count <= 1 then
      Exit;

   // PolyBezier expects count*3-2 points.
   len_pt := count * 3 - 2;
   SetLength(pt, len_pt);
   tension := tension * 0.3;

   calc_curve_bezier_endp(points[0].X, points[0].Y, points[1].X, points[1].Y,
      tension, x1, y1);

   pt[0] := points[0];
   pt[1].X := x1;
   pt[1].Y := y1;

   for i := 0 to count - 3 do
   begin
      calc_curve_bezier(TGLPointsF(@(points[i])), tension, x1, y1, x2, y2);
      pt[3 * i + 2].X := x1;
      pt[3 * i + 2].Y := y1;
      pt[3 * i + 3] := points[i + 1];
      pt[3 * i + 4].X := x2;
      pt[3 * i + 4].Y := y2;
   end;

   calc_curve_bezier_endp(points[count - 1].X, points[count - 1].Y,
       points[count - 2].X, points[count - 2].Y, tension, x1, y1);
   pt[len_pt - 2].X := x1;
   pt[len_pt - 2].Y := y1;
   pt[len_pt - 1] := points[count - 1];

   GDI_Bezier(pt, len_pt, outPoints, outCount);
end;

function TGLCanvas.Curve(const points: TGLPointsF; count: Integer;
   tension: Single = 0.5): TGLCanvas;
var
   pt2: TGLPointsF;
   pt2Count: Integer;
begin
   Result := Self;
   if count <= 1 then
      Exit;

   GenCurvePoints(points, count, pt2, pt2Count, tension);
   Polyline(pt2, pt2Count);
end;

function TGLCanvas.Curve(const points: TGLPointsI; count: Integer;
   tension: Single = 0.5): TGLCanvas;
var
   i: Integer;
   pfs: TGLPointsF;
begin
   Result := Self;
   if count <= 1 then
      Exit;

   SetLength(pfs, count);
   for i := 0 to count - 1 do
   begin
      pfs[i].X := points[i].X;
      pfs[i].Y := points[i].Y;
   end;
   Curve(pfs, count, tension);
end;

function TGLCanvas.ClosedCurve(const points: TGLPointsF; count: Integer;
   tension: Single = 0.5): TGLCanvas;
var
   ps: TGLPointsF;
begin
   Result := Self;
   if count <= 2 then
      Exit;

   SetLength(ps, count + 1);
   Move(points[0], ps[0], SizeOf(TGLPointF) * count);
   ps[count] := ps[0]; // Close the curve
   Curve(ps, count + 1, tension);
end;

function TGLCanvas.ClosedCurve(const points: TGLPointsI; count: Integer;
   tension: Single = 0.5): TGLCanvas;
var
   i: Integer;
   ps: TGLPointsF;
begin
   Result := Self;
   if count <= 2 then
      Exit;

   SetLength(ps, count + 1);
   for i := 0 to count - 1 do
   begin
      ps[i].X := points[i].X;
      ps[i].Y := points[i].Y;
   end;
   ps[count] := ps[0]; // Close the curve
   Curve(ps, count + 1, tension);
end;

function TGLCanvas.FillClosedCurve(const points: TGLPointsF; count: Integer;
   Border: Boolean = False; tension: Single = 0.5): TGLCanvas;
var
   ps, pt2: TGLPointsF;
   pt2Count: Integer;
begin
   Result := Self;
   if count <= 2 then
      Exit;

   SetLength(ps, count + 1);
   Move(points[0], ps[0], SizeOf(TGLPointF) * count);
   ps[count] := ps[0]; // Close the curve

   GenCurvePoints(ps, count + 1, pt2, pt2Count, tension);
   FillPolygon(pt2, pt2Count, Border);
end;

function TGLCanvas.FillClosedCurve(const points: TGLPointsI; count: Integer;
   Border: Boolean = False; tension: Single = 0.5): TGLCanvas;
var
   i: Integer;
   ps, pt2: TGLPointsF;
   pt2Count: Integer;
begin
   Result := Self;
   if count <= 2 then
      Exit;

   SetLength(ps, count + 1);
   for i := 0 to count - 1 do
   begin
      ps[i].X := points[i].X;
      ps[i].Y := points[i].Y;
   end;
   ps[count] := ps[0]; // Close the curve

   GenCurvePoints(ps, count + 1, pt2, pt2Count, tension);
   FillPolygon(pt2, pt2Count, Border);
end;

function TGLCanvas.Bezier(const x1, y1, x2, y2, x3, y3, x4, y4: Integer): TGLCanvas;
var
   pt: TGDIBezierPoints;
   pt2: TGLPointsF;
   ptOut: Integer;
begin
   pt[0].X := x1;
   pt[0].Y := y1;
   pt[1].X := x2;
   pt[1].Y := y2;
   pt[2].X := x3;
   pt[2].Y := y3;
   pt[3].X := x4;
   pt[3].Y := y4;

   GDI_Bezier(TGLPointsF(@pt[0]), 4, pt2, ptOut);
   Polyline(pt2, ptOut);
   Result := Self;
end;

function TGLCanvas.Bezier(const x1, y1, x2, y2, x3, y3, x4, y4: Single): TGLCanvas;
var
   pt: TGDIBezierPoints;
   pt2: TGLPointsF;
   ptOut: Integer;
begin
   pt[0].X := x1;
   pt[0].Y := y1;
   pt[1].X := x2;
   pt[1].Y := y2;
   pt[2].X := x3;
   pt[2].Y := y3;
   pt[3].X := x4;
   pt[3].Y := y4;

   GDI_Bezier(TGLPointsF(@pt[0]), 4, pt2, ptOut);
   Polyline(pt2, ptOut);
   Result := Self;
end;

function TGLCanvas.PolyBezier(const points: TGLPointsI; count: Integer): TGLCanvas;
var
   i: Integer;
   ps, pt2: TGLPointsF;
   pt2Count: Integer;
begin
   SetLength(ps, count);
   for i := 0 to count - 1 do
   begin
      ps[i].X := points[i].X;
      ps[i].Y := points[i].Y;
   end;

   GDI_Bezier(ps, count, pt2, pt2Count);
   Polyline(pt2, pt2Count);
   Result := Self;
end;

function TGLCanvas.PolyBezier(const points: TGLPointsF; count: Integer): TGLCanvas;
var
   ps2Count: Integer;
   ps2: TGLPointsF;
begin
   GDI_Bezier(points, count, ps2, ps2Count);
   Polyline(ps2, ps2Count);
   Result := Self;
end;

const
   MAX_ARC_PTS = 13;
type
   TGdiArcPoints = array[0..MAX_ARC_PTS - 1] of TGLPointF;

   PGdiArcPointsSegment = ^TGdiArcPointsSegment;
   TGdiArcPointsSegment = array[0..3] of TGLPointF;

{ We plot the curve as if it is on a circle then stretch the points.  This
  adjusts the angles so that when we stretch the points they will end in the
  right place. This is only complicated because atan and atan2 do not behave
  conveniently. }
procedure unstretch_angle(var angle: Single; rad_x, rad_y: Single);
var
   stretched: Single;
   revs_off: Integer;
begin
    angle := DegToRad(angle);

    if(Abs(Cos(angle)) < 0.00001) or (Abs(Sin(angle)) < 0.00001) then
       Exit;

    stretched := ArcTan2(Sin(angle) / Abs(rad_y), Cos(angle) / Abs(rad_x));
    revs_off := Round(angle / _2Pi) - Round(stretched / _2Pi);
    angle := stretched + revs_off * _2Pi;
end;

{ Calculates the bezier points needed to fill in the arc portion starting at
  angle start and ending at end.  These two angles should be no more than 90
  degrees from each other.  x1, y1, x2, y2 describes the bounding box (upper
  left and width and height).  Angles must be in radians. write_first indicates
  that the first bezier point should be written out (usually this is false).
  pt is the array of GpPointFs that gets written to. }
procedure add_arc_part(pt: PGdiArcPointsSegment; const x1, y1, x2, y2: Single;
   startangle, endangle: Single; write_first: Boolean);
var
   i: Integer;
   center_x, center_y, rad_x, rad_y, cos_start, cos_end,
      sin_start, sin_end, a, half: Single;
begin
    rad_x := x2 / 2.0;
    rad_y := y2 / 2.0;
    center_x := x1 + rad_x;
    center_y := y1 + rad_y;

    SinCos(startangle, sin_start, cos_start);
    SinCos(endangle, sin_end, cos_end);

    half := (endangle - startangle) / 2.0;
    a := 4.0 / 3.0 * (1 - Cos(half)) / Sin(half);

    if write_first then
    begin
       pt^[0].X := cos_start;
       pt^[0].Y := sin_start;
    end;
    pt^[1].X := cos_start - a * sin_start;
    pt^[1].Y := sin_start + a * cos_start;

    pt^[3].X := cos_end;
    pt^[3].Y := sin_end;
    pt^[2].X := cos_end + a * sin_end;
    pt^[2].Y := sin_end - a * cos_end;

    // expand the points back from the unit circle to the ellipse
    if write_first then
    begin
       for i := 0 to 3 do
       begin
          pt^[i].X := pt^[i].X * rad_x + center_x;
          pt^[i].Y := pt^[i].Y * rad_y + center_y;
       end;
    end
    else
    begin
       for i := 1 to 3 do
       begin
          pt^[i].X := pt^[i].X * rad_x + center_x;
          pt^[i].Y := pt^[i].Y * rad_y + center_y;
       end;
    end;
end;

{ Stores the bezier points that correspond to the arc in points. If points is
  null, just return the number of points needed to represent the arc. }
function arc2polybezier(var points: TGdiArcPoints; const x1, y1, x2, y2: Single;
   var startAngle, sweepAngle: Single): Integer;
var
   i, count: Integer;
   end_angle, start_angle, endAngle: Single;
begin
    endAngle := startAngle + sweepAngle;
    unstretch_angle(startAngle, x2 / 2.0, y2 / 2.0);
    unstretch_angle(endAngle, x2 / 2.0, y2 / 2.0);

    count := Ceil(Abs(endAngle - startAngle) / PiDiv2) * 3 + 1;
    count := Min(MAX_ARC_PTS, count); // don't make more than a full circle

    if count = 1 then
    begin
       Result := 0;
       Exit;
    end;

    // start_angle and end_angle are the iterative variables
    start_angle := startAngle;
    i := 0;
    while (i < count - 1) do
    begin
       // check if we've overshot the end angle
       if sweepAngle > 0.0 then
          end_angle := Min(start_angle + PiDiv2, endAngle)
       else
          end_angle := Max(start_angle - PiDiv2, endAngle);

       if SameValue(start_angle, end_angle) then
       begin
          count := i + 1;
          Break;
       end;

       add_arc_part(PGdiArcPointsSegment(@points[i]), x1, y1, x2, y2,
          start_angle, end_angle, i = 0);

       if sweepAngle < 0.0 then
          start_angle := start_angle - PiDiv2
       else
          start_angle := start_angle + PiDiv2;
       i := i + 3;
    end;

    Result := count;
end;

function TGLCanvas.Arc(const x, y, xRadius, yRadius: Single;
   startAngle, sweepAngle: Single): TGLCanvas;
var
   num_pts: Integer;
   points: TGdiArcPoints;
begin
   num_pts := arc2polybezier(points, x - xRadius, y - yRadius,
      xRadius * 2, yRadius * 2, startAngle, sweepAngle);
   PolyBezier(TGLPointsF(@points[0]), num_pts);
   Result := Self;
end;

function TGLCanvas.FillPie(const x, y, xRadius, yRadius: Single;
   startAngle, sweepAngle: Single; Border: Boolean = False): TGLCanvas;
var
   i, num_pts: Integer;
   points: TGdiArcPoints;
   ps2Count: Integer;
   ps2: TGLPointsF;
begin
   if not FIgnoreColor then
      glColor4fv(@FBrushColor);
   glBegin(GL_TRIANGLE_FAN);
   glVertex2f(x, y); // not really necessary, but may help with memory stride

   num_pts := arc2polybezier(points, x - xRadius, y - yRadius,
      xRadius * 2, yRadius * 2, startAngle, sweepAngle);
   GDI_Bezier(TGLPointsF(@points[0]), num_pts, ps2, ps2Count);
   for i := 0 to ps2Count - 1 do
      glVertex2f(ps2[i].X, ps2[i].Y);

   glEnd;
   if not FIgnoreColor then
      glColor4fv(@FPenColor);

   if Border then
   begin
      glBegin(GL_LINE_STRIP);
      glVertex2f(x, y);
      for i := 0 to ps2Count - 1 do
         glVertex2f(ps2[i].X, ps2[i].Y);
      glVertex2f(x, y);
      glEnd;
   end;

   Result := Self;
end;

function TGLCanvas.PlotPixel(const x, y: Integer): TGLCanvas;
begin
   glBegin(GL_POINTS);
   glVertex2i(x, y);
   glEnd;
   Result := Self;
end;

function TGLCanvas.PlotPixel(const x, y: Single): TGLCanvas;
begin
   glBegin(GL_POINTS);
   glVertex2f(x, y);
   glEnd;
   Result := Self;
end;

function TGLCanvas.BeginPixels: TGLCanvas;
begin
   glBegin(GL_POINTS);
   Result := Self;
end;

function TGLCanvas.Pixels(const x, y: Integer): TGLCanvas;
begin
   glVertex2i(x, y);
   Result := Self;
end;

function TGLCanvas.Pixels(const x, y: Single): TGLCanvas;
begin
   glVertex2f(x, y);
   Result := Self;
end;

function TGLCanvas.EndPixels: TGLCanvas;
begin
   glEnd;
   Result := Self;
end;

function TGLCanvas.FrameRect(const x1, y1, x2, y2: Integer): TGLCanvas;
begin
   glBegin(GL_LINE_LOOP);
   glVertex2i(x1, y1);
   glVertex2i(x2, y1);
   glVertex2i(x2, y2);
   glVertex2i(x1, y2);
   glEnd;
   Result := Self;
end;

function TGLCanvas.FrameRect(const x1, y1, x2, y2: Single): TGLCanvas;
begin
   glBegin(GL_LINE_LOOP);
   glVertex2f(x1, y1);
   glVertex2f(x2, y1);
   glVertex2f(x2, y2);
   glVertex2f(x1, y2);
   glEnd;
   Result := Self;
end;

function TGLCanvas.FillRect(const x1, y1, x2, y2: Integer; Border: Boolean = False): TGLCanvas;
begin
   if not FIgnoreColor then 
      glColor4fv(@FBrushColor);
   glRecti(x1, y1, x2, y2);
   if not FIgnoreColor then 
      glColor4fv(@FPenColor);
   if Border then
      FrameRect(x1, y1, x2, y2);
   Result := Self;
end;

function TGLCanvas.FillRect(const x1, y1, x2, y2: Single; Border: Boolean = False): TGLCanvas;
begin
   if not FIgnoreColor then 
      glColor4fv(@FBrushColor);
   glRectf(x1, y1, x2, y2);
   if not FIgnoreColor then 
      glColor4fv(@FPenColor);
   if Border then
      FrameRect(x1, y1, x2, y2);
   Result := Self;
end;

function TGLCanvas.Triangle(const x1, y1, x2, y2, x3, y3: Integer): TGLCanvas;
begin
   glBegin(GL_LINE_LOOP);
   glVertex2i(x1, y1);
   glVertex2i(x2, y2);
   glVertex2i(x3, y3);
   glEnd;
   Result := Self;
end;

function TGLCanvas.Triangle(const x1, y1, x2, y2, x3, y3: Single): TGLCanvas;
begin
   glBegin(GL_LINE_LOOP);
   glVertex2f(x1, y1);
   glVertex2f(x2, y2);
   glVertex2f(x3, y3);
   glEnd;
   Result := Self;
end;

function TGLCanvas.FillTriangle(const x1, y1, x2, y2, x3, y3: Integer;
   Border: Boolean = False): TGLCanvas;
begin
   if not FIgnoreColor then 
      glColor4fv(@FBrushColor);
   glBegin(GL_TRIANGLE_FAN);
   glVertex2i(x1, y1);
   glVertex2i(x2, y2);
   glVertex2i(x3, y3);
   glEnd;
   if not FIgnoreColor then 
      glColor4fv(@FPenColor);
   if Border then
      Triangle(x1, y1, x2, y2, x3, y3);
   Result := Self;
end;

function TGLCanvas.FillTriangle(const x1, y1, x2, y2, x3, y3: Single;
   Border: Boolean = False): TGLCanvas;
begin
   if not FIgnoreColor then 
      glColor4fv(@FBrushColor);
   glBegin(GL_TRIANGLE_FAN);
   glVertex2f(x1, y1);
   glVertex2f(x2, y2);
   glVertex2f(x3, y3);
   glEnd;
   if not FIgnoreColor then 
      glColor4fv(@FPenColor);
   if Border then
      Triangle(x1, y1, x2, y2, x3, y3);
   Result := Self;
end;

// Ellipse drawing methods are borrowed from GLScene.GLCanvas unit

procedure PrepareSinCosCache(var s, c: array of Single;
   startAngle, stopAngle: Single);
var
   i: Integer;
   d, alpha, beta: Single;
begin
   stopAngle := stopAngle + 1E-5;
   if High(s) > Low(s) then
      d := PiDiv180 * (stopAngle - startAngle) / (High(s) - Low(s))
   else
      d := 0;

   if High(s) - Low(s) < 1000 then
   begin
      // Fast computation (approx 5.5x)
      alpha := 2 * Sqr(Sin(d * 0.5));
      beta := Sin(d);
      SinCos(startAngle * PiDiv180, s[Low(s)], c[Low(s)]);
      for i := Low(s) to High(s) - 1 do
      begin
         // Make use of the incremental formulae:
         // cos (theta+delta) = cos(theta) - [alpha*cos(theta) + beta*sin(theta)]
         // sin (theta+delta) = sin(theta) - [alpha*sin(theta) - beta*cos(theta)]
         c[i + 1] := c[i] - alpha * c[i] - beta * s[i];
         s[i + 1] := s[i] - alpha * s[i] + beta * c[i];
      end;
   end
   else
   begin
      // Slower, but maintains precision when steps are small
      startAngle := startAngle * PiDiv180;
      for i := Low(s) to High(s) do
         SinCos((i - Low(s)) * d + startAngle, s[i], c[i]);
   end;
end;

procedure TGLCanvas.EllipseVertices(const x, y, xRadius, yRadius: Single);
var
   i, n: Integer;
   s, c: TSingleArray;
begin
   if xRadius > yRadius then
      n := Round(xRadius * 0.1) + 10
   else
      n := Round(yRadius * 0.1) + 10;
   SetLength(s, n);
   SetLength(c, n);
   Dec(n);
   PrepareSinCosCache(s, c, 0, 90);
   // first quadrant (top right)
   for i := 0 to n do
   begin
      s[i] := s[i] * yRadius;
      c[i] := c[i] * xRadius;
      glVertex2f(x + c[i], y - s[i]);
   end;
   // second quadrant (top left)
   for i := n - 1 downto 0 do
      glVertex2f(x - c[i], y - s[i]);
   // third quadrant (bottom left)
   for i := 1 to n do
      glVertex2f(x - c[i], y + s[i]);
   // fourth quadrant (bottom right)
   for i := n - 1 downto 0 do
      glVertex2f(x + c[i], y + s[i]);
end;

function TGLCanvas.EllipseRect(const x1, y1, x2, y2: Single): TGLCanvas;
begin
   Ellipse((x1 + x2) * 0.5, (y1 + y2) * 0.5, Abs(x2 - x1) * 0.5, Abs(y2 - y1) * 0.5);
   Result := Self;
end;

function TGLCanvas.Ellipse(const x, y, xRadius, yRadius: Single): TGLCanvas;
begin
   glBegin(GL_LINE_STRIP);
   EllipseVertices(x, y, xRadius, yRadius);
   glEnd;
   Result := Self;
end;

function TGLCanvas.FillEllipseRect(const x1, y1, x2, y2: Single;
   Border: Boolean = False): TGLCanvas;
begin
   FillEllipse((x1 + x2) * 0.5, (y1 + y2) * 0.5, Abs(x2 - x1) * 0.5, Abs(y2 - y1) * 0.5, Border);
   Result := Self;
end;

function TGLCanvas.FillEllipse(const x, y, xRadius, yRadius: Single;
   Border: Boolean = False): TGLCanvas;
begin
   if not FIgnoreColor then
      glColor4fv(@FBrushColor);
   glBegin(GL_TRIANGLE_FAN);
   glVertex2f(x, y); // not really necessary, but may help with memory stride
   EllipseVertices(x, y, xRadius, yRadius);
   glEnd;
   if not FIgnoreColor then 
      glColor4fv(@FPenColor);
   if Border then
      Ellipse(x, y, xRadius, yRadius);
   Result := Self;
end;

procedure TGLCanvas.RecreateDefaultFont;
begin
   if Assigned(FDefaultFont) then
      FDefaultFont.Free;
   FDefaultFont := TGLFont.Create('Tahoma', 14);
   FDefaultFont.NotifyChange := DefaultFontNotify; // Mustn't assign NotifyChange by TCnGLFont.Create
end;

function TGLCanvas.TextOutASCII(const text: string; x, y: Integer;
   Font: TGLFont = nil): TGLCanvas;
var
   i: Integer;
   GLList: GLuint;
   NeedFreeList: Boolean;
begin
  if Assigned(Font) then
  begin
    SelectObject(FBufferHDC, Font.HFont);
    GLList := glGenLists(MaxChar);
    wglUseFontBitmaps(FBufferHDC, 0, MaxChar, GLList);
    NeedFreeList := True;
  end
  else
  begin
    Font := FDefaultFont;
    GLList := FASCIICharList;
    NeedFreeList := False;
  end;

  glPushMatrix;
  glLoadIdentity;
  glColor4fv(@Font.FColorVector);
  glRasterPos2i(x, y);
  for i := 1 to Length(text) do
    glCallList(GLList + Ord(text[i]));
  glPopMatrix;

  if NeedFreeList then
    glDeleteLists(GLList, MaxChar);
  glColor4fv(@FPenColor);
  Result := Self;
end;

function TGLCanvas.TextOut(const text: WideString; x, y: Integer;
   Font: TGLFont = nil): TGLCanvas;
var
  i: Integer;
  list: GLuint;
begin
  if not Assigned(Font) then
    Font := FDefaultFont;
   SelectObject(FBufferHDC, Font.HFont);
   glColor4fv(@Font.FColorVector);

   glPushMatrix;
   glLoadIdentity;
   glRasterPos2i(x, y);
   for i := 1 to Length(text) do
   begin
      wglUseFontBitmapsW(FBufferHDC, Ord(text[i]), 1, list);
      glCallList(list);
   end;
   glDeleteLists(list, 1);
   glColor4fv(@FPenColor);
   glPopMatrix;
   Result := Self;
end;

function TGLCanvas.BuildTexture(bmp: TBitmap; var texId: GLuint): TGLCanvas; // Creates Texture From A Bitmap File
var
   bmpInfo: BITMAP;
begin
   GetObject(bmp.Handle, SizeOf(bmpInfo), @bmpInfo);
   glGenTextures(1, @texId);          // Create The Texture
   glPixelStorei(GL_PACK_ALIGNMENT, 1);
   // Typical Texture Generation Using Data From The Bitmap
   glBindTexture(GL_TEXTURE_2D, texId);        // Bind To The Texture ID
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR); // Linear Min Filter
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR); // Linear Mag Filter
   glTexImage2D(GL_TEXTURE_2D, 0, 3, bmpInfo.bmWidth, bmpInfo.bmHeight, 0, GL_BGR, GL_UNSIGNED_BYTE, bmpInfo.bmBits);
   Result := Self;
end;

function TGLCanvas.DeleteTexture(texId: GLuint): TGLCanvas;
begin
   glDeleteTextures(1, @texId);
   Result := Self;
end;

function TGLCanvas.DrawBitmap(bmp: TBitmap; x, y: Integer;
   xZoom: Single = 1.0; yZoom: Single = 1.0): TGLCanvas;
var
   bmpInfo: BITMAP;
begin
   GetObject(bmp.Handle, SizeOf(bmpInfo), @bmpInfo);
   glPixelZoom(xZoom, yZoom);
   glPushMatrix;
   glLoadIdentity;
   glRasterPos2i(x, y);
   glDrawPixels(bmp.Width, bmp.Height, GL_BGR, GL_UNSIGNED_BYTE, bmpInfo.bmBits);
   glPopMatrix;
   Result := Self;
end;

function TGLCanvas.DrawBitmapTex(bmp: TBitmap; x, y, w, h: Integer): TGLCanvas;
var
   tex: GLuint;
begin
   glColor3f(1.0, 1.0, 1.0);
   glDisable(GL_BLEND);
   glEnable(GL_TEXTURE_2D);
   BuildTexture(bmp, tex);

   glBegin(GL_QUADS);
   glTexCoord2f(0.0, 0.0); glVertex3i(x, y, 0);
   glTexCoord2f(1.0, 0.0); glVertex3f(x + w, y, 0);
   glTexCoord2f(1.0, 1.0); glVertex3f(x + w, y + h, 0);
   glTexCoord2f(0.0, 1.0); glVertex3f(x, y + h, 0);
   glEnd;

   glDeleteTextures(1, @tex);
   glDisable(GL_TEXTURE_2D);
   SetBlendState(FBlend);
   glColor4fv(@FPenColor); // Restore color
   Result := Self;
end;

function TGLCanvas.DrawBitmapTex(texId: GLuint; x, y, w, h: Integer): TGLCanvas;
begin
   glColor3f(1.0, 1.0, 1.0);
   glDisable(GL_BLEND);
   glEnable(GL_TEXTURE_2D);

   glBindTexture(GL_TEXTURE_2D, texid);        // Bind To The Texture ID
   glBegin(GL_QUADS);
   glTexCoord2f(0.0, 0.0); glVertex3i(x, y, 0);
   glTexCoord2f(1.0, 0.0); glVertex3f(x + w, y, 0);
   glTexCoord2f(1.0, 1.0); glVertex3f(x + w, y + h, 0);
   glTexCoord2f(0.0, 1.0); glVertex3f(x, y + h, 0);
   glEnd;

   glDisable(GL_TEXTURE_2D);
   SetBlendState(FBlend);
   glColor4fv(@FPenColor); // Restore color
   Result := Self;
end;

end.

