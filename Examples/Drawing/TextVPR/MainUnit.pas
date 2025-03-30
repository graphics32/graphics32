unit MainUnit;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1 or LGPL 2.1 with linking exception
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * Alternatively, the contents of this file may be used under the terms of the
 * Free Pascal modified version of the GNU Lesser General Public License
 * Version 2.1 (the "FPC modified LGPL License"), in which case the provisions
 * of this license are applicable instead of those above.
 * Please see the file LICENSE.txt for additional information concerning this
 * license.
 *
 * The Original Code is TextDemoVPR Example (based on VPR example)
 *
 * The Initial Developer of the Original Code is
 * Mattias Andersson <mattias@centaurix.com>
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2005
 * the Initial Developer. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

interface

{$include GR32.inc}

uses
{$ifdef FPC}
  LCLIntf, LResources, LCLType,
{$endif}
{$ifdef WINDOWS}
  Windows,
{$endif}
  Messages, SysUtils,
  Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Buttons,
  ComCtrls,

  GR32, GR32_Paths, GR32_Image, GR32_Layers, GR32.Text.Types;

{$ifdef WINDOWS}
type
  TTrackBar = class(ComCtrls.TTrackBar)
  protected
    procedure WndProc(var Message: TMessage); override;
{$ifdef FPC}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
{$endif}
  end;
{$endif}

type
  TMainForm = class(TForm)
    ButtonExit: TButton;
    ButtonSelectFont: TButton;
    CheckBoxSingleLine: TCheckBox;
    CheckBoxWordbreak: TCheckBox;
    FontDialog: TFontDialog;
    GroupBoxFont: TGroupBox;
    GroupBoxLayout: TGroupBox;
    GroupBoxRendering: TGroupBox;
    Img: TImage32;
    LblFontInfo: TLabel;
    PaintBox32: TPaintBox32;
    PnlControl: TPanel;
    PnlImage: TPanel;
    PnlZoom: TPanel;
    RadioGroupMethod: TRadioGroup;
    StatusBar: TStatusBar;
    TrackBarGamma: TTrackBar;
    GroupBoxGamma: TGroupBox;
    PanelLeft: TPanel;
    CheckBoxKerning: TCheckBox;
    GroupBoxAlignHorizontal: TGroupBox;
    GroupBoxAlignVertical: TGroupBox;
    GroupBoxJustification: TGroupBox;
    TrackBarZoom: TTrackBar;
    PanelAlignHor: TPanel;
    ButtonAlignHorLeft: TSpeedButton;
    ButtonAlignHorCenter: TSpeedButton;
    ButtonAlignHorRight: TSpeedButton;
    ButtonAlignHorJustify: TSpeedButton;
    PanelAlignVer: TPanel;
    ButtonAlignVerTop: TSpeedButton;
    ButtonAlignVerCenter: TSpeedButton;
    ButtonAlignVerBottom: TSpeedButton;
    PanelJustification: TPanel;
    Label1: TLabel;
    TrackBarInterChar: TTrackBar;
    Label3: TLabel;
    TrackBarInterWordMax: TTrackBar;
    GroupBoxClipping: TGroupBox;
    CheckBoxClipRaster: TCheckBox;
    CheckBoxClipLayout: TCheckBox;
    Shape1: TShape;
    ComboBoxExample: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonExitClick(Sender: TObject);
    procedure ButtonSelectFontClick(Sender: TObject);
    procedure ImgClick(Sender: TObject);
    procedure ImgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer;
      Layer: TCustomLayer);
    procedure RadioGroupMethodClick(Sender: TObject);
    procedure TrackBarGammaChange(Sender: TObject);
    procedure TrackBarInterCharChange(Sender: TObject);
    procedure TrackBarInterWordMaxChange(Sender: TObject);
    procedure ImgMouseLeave(Sender: TObject);
    procedure ButtonAlignHorClick(Sender: TObject);
    procedure ButtonAlignVerClick(Sender: TObject);
    procedure TrackBarZoomChange(Sender: TObject);
    procedure DoLayoutAndRender(Sender: TObject);
    procedure DoRender(Sender: TObject);
  private
    FCanvas: TCanvas32;
    FTextLayout: TTextLayout;
    FApplyOptions: boolean;
    FLayoutTime: Int64;
    FRenderTime: Int64;

  private
    procedure BuildPolygonFromText;
    procedure RenderText;
    procedure DisplayFontInfo;
    procedure DrawZoom(X, Y: integer);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
{$ifndef FPC}
  Types,
  System.UITypes,
{$endif}

{$ifdef WINDOWS}
  CommCtrl,
{$ifdef FPC}
  Win32Proc,
{$endif}
{$endif}

  Math,
  GR32_System,
  GR32_Gamma,
  GR32_Backends,
  GR32_Polygons,
  GR32_Brushes;

const
  sMobyDick = // ASCII breaks
    'Moby Dick'+ #13 +
    'Herman Melville'+ #13#13 +
    // Leading space
    '  Call me Ishmael. Some years ago—never mind how long precisely—having little or no money in my purse, and nothing '+
    'particular to interest me on shore, I thought I would sail about a little and see the watery part of the world. It '+
    'is a way I have of driving off the spleen and regulating the circulation. Whenever I find myself growing grim about '+
    'the mouth; whenever it is a damp, drizzly November in my soul; whenever I find myself involuntarily pausing before '+
    'coffin warehouses, and bringing up the rear of every funeral I meet; and especially whenever my hypos get such an '+
    'upper hand of me, that it requires a strong moral principle to prevent me from deliberately stepping into the '+
    'street, and methodically knocking people’s hats off—then, I account it high time to get to sea as soon as I can. '+
    'This is my substitute for pistol and ball. With a philosophical flourish Cato throws himself upon his sword; I '+
    'quietly take to the ship. There is nothing surprising in this. If they but knew it, almost all men in their degree, '+
    'some time or other, cherish very nearly the same feelings towards the ocean with me. '+ // Trailing space
    // Two line breaks as paragraph separator
    #13 +
    #13 +
    '  There now is your insular city of the Manhattoes, belted round by wharves as Indian isles by coral reefs—commerce '+
    'surrounds it with her surf. Right and left, the streets take you waterward. Its extreme downtown is the battery, '+
    'where that noble mole is washed by waves, and cooled by breezes, which a few hours previous were out of sight of '+
    'land. Look at the crowds of water-gazers there.'+
    // Two line breaks as paragraph separator
    #10 +
    #10 +
    '  Circumambulate the city of a dreamy Sabbath afternoon. Go from Corlears Hook to Coenties Slip, and from thence, '+
    'by Whitehall, northward. What do you see?—Posted like silent sentinels all around the town, stand thousands upon '+
    'thousands of mortal men fixed in ocean reveries. Some leaning against the spiles; some seated upon the pier-heads; '+
    'some looking over the bulwarks of ships from China; some high aloft in the rigging, as if striving to get a still '+
    'better seaward peep. But these are all landsmen; of week days pent up in lath and plaster—tied to counters, nailed '+
    'to benches, clinched to desks. How then is this? Are the green fields gone? What do they here?';

  sMaBohème = // Mixed breaks and No-break spaces
    'Ma'#$00A0'bohème'+ #$2028 +
    'Arthur'#$00A0'Rimbaud'+ #$2029 +
    // Unicode line separators
      'Je m’en allais, les poings dans mes poches crevées'#$00A0';'+ #$2028 +
      'Mon paletot aussi devenait idéal'#$00A0';'+ #$2028 +
      'J’allais sous le ciel, Muse'#$00A0'! et j’étais ton féal ;'+ #$2028 +
      'Oh'#$00A0'! là'#$00A0'! là'#$00A0'! que d’amours splendides j’ai rêvées'#$00A0'!'+
    //Paragraph separator
    #$2029 +
    // ASCII line separators
      'Mon unique culotte avait un large trou.'+ #13 +
      '–'#$00A0'Petit-Poucet rêveur, j’égrenais dans ma course'+ #13 +
      'Des rimes. Mon auberge était à la Grande-Ourse.'+ #13 +
      '–'#$00A0'Mes étoiles au ciel avaient un doux frou-frou'+
    // Double line break
    #10 + #10 +
    // ASCII line separators
      'Et je les écoutais, assis au bord des routes,'+ #10 +
      'Ces bons soirs de septembre où je sentais des gouttes'+ #10 +
      'De rosée à mon front, comme un vin de vigueur'#$00A0';'+
    // Double line break
    #13 + #13 +
    // ASCII line separators
      'Où, rimant au milieu des ombres fantastiques,'+ #13#10 +
      'Comme des lyres, je tirais les élastiques'+ #13#10 +
      'De mes souliers blessés, un pied près de mon coeur'#$00A0'!';

  sDonQuixote = // Unicode breaks
    'Don Quixote'+ #$2028 +
    'Miguel de Cervantes'+ #$2029 +
      'En un lugar de la Mancha, de cuyo nombre no quiero acordarme, no ha mucho tiempo que vivía un hidalgo de los '+
      'de lanza en astillero, adarga antigua, rocín flaco y galgo corredor. Una olla de algo más vaca que carnero, '+
      'salpicón las más noches, duelos y quebrantos los sábados, lantejas los viernes, algún palomino de añadidura '+
      'los domingos, consumían las tres partes de su hacienda. El resto della concluían sayo de velarte, calzas de '+
      'velludo para las fiestas, con sus pantuflos de lo mesmo, y los días de entresemana se honraba con su vellorí '+
      'de lo más fino. Tenía en su casa una ama que pasaba de los cuarenta, y una sobrina que no llegaba a los '+
      'veinte, y un mozo de campo y plaza, que así ensillaba el rocín como tomaba la podadera. Frisaba la edad de '+
      'nuestro hidalgo con los cincuenta años; era de complexión recia, seco de carnes, enjuto de rostro, gran '+
      'madrugador y amigo de la caza. Quieren decir que tenía el sobrenombre de Quijada, o Quesada, que en esto hay '+
      'alguna diferencia en los autores que deste caso escriben; aunque, por conjeturas verosímiles, se deja '+
      'entender que se llamaba Quejana. Pero esto importa poco a nuestro cuento; basta que en la narración dél no se '+
      'salga un punto de la verdad.'+
    //Paragraph separator
    #$2029 +
      'Es, pues, de saber que este sobredicho hidalgo, los ratos que estaba ocioso, que eran los más del año, se '+
      'daba a leer libros de caballerías, con tanta afición y gusto, que olvidó casi de todo punto el ejercicio de '+
      'la caza, y aun la administración de su hacienda. Y llegó a tanto su curiosidad y desatino en esto, que vendió '+
      'muchas hanegas de tierra de sembradura para comprar libros de caballerías en que leer, y así, llevó a su casa '+
      'todos cuantos pudo haber dellos; y de todos, ningunos le parecían tan bien como los que compuso el famoso '+
      'Feliciano de Silva, porque la claridad de su prosa y aquellas entricadas razones suyas le parecían de perlas, '+
      'y más cuando llegaba a leer aquellos requiebros y cartas de desafíos, donde en muchas partes hallaba escrito: '+
      'La razón de la sinrazón que a mi razón se hace, de tal manera mi razón enflaquece, que con razón me quejo de '+
      'la vuestra fermosura. Y también cuando leía: ...los altos cielos que de vuestra divinidad divinamente con las '+
      'estrellas os fortifican, y os hacen merecedora del merecimiento que merece la vuestra grandeza.'+
    //Paragraph separator
    #$2029 +
      'Con estas razones perdía el pobre caballero el juicio, y desvelábase por entenderlas y desentrañarles el '+
      'sentido, que no se lo sacara ni las entendiera el mesmo Aristóteles, si resucitara para sólo ello. No estaba '+
      'muy bien con las heridas que don Belianís daba y recebía, porque se imaginaba que, por grandes maestros que '+
      'le hubiesen curado, no dejaría de tener el rostro y todo el cuerpo lleno de cicatrices y señales. Pero, con '+
      'todo, alababa en su autor aquel acabar su libro con la promesa de aquella inacabable aventura, y muchas veces '+
      'le vino deseo de tomar la pluma y dalle fin al pie de la letra, como allí se promete; y sin duda alguna lo '+
      'hiciera, y aun saliera con ello, si otros mayores y continuos pensamientos no se lo estorbaran. Tuvo muchas '+
      'veces competencia con el cura de su lugar —que era hombre docto, graduado en Sigüenza—, sobre cuál había sido '+
      'mejor caballero: Palmerín de Ingalaterra o Amadís de Gaula; mas maese Nicolás, barbero del mesmo pueblo, '+
      'decía que ninguno llegaba al Caballero del Febo, y que si alguno se le podía comparar, era don Galaor, '+
      'hermano de Amadís de Gaula, porque tenía muy acomodada condición para todo; que no era caballero melindroso, '+
      'ni tan llorón como su hermano, y que en lo de la valentía no le iba en zaga.';

  sDoOxenLowWhenMangersAreFull = // Mixed breaks and No-break spaces
    'Хіба ревуть воли, як ясла повні?'+ #13 +
    'Panas Myrny, Ivan Bilyk'+ #$2029 +
      'Надворі весна вповні. Куди не глянь — скрізь розвернулося, розпустилося, зацвіло пишним цвітом. Ясне сонце, '+
      'тепле й приязне, ще не вспіло наложити палючих слідів на землю: як на Великдень дівчина, красується вона в '+
      'своїм розкішнім убранні... Поле'#$00A0'— що безкрає море'#$00A0'— скільки зглянеш'#$00A0'— розіслало зелений килим, аж сміється в '+
      'очах. Над ним синім шатром розіп''ялось небо'#$00A0'— ні плямочки, ні хмарочки, чисте, прозоре'#$00A0'— погляд так і тоне... '+
      'З неба, як розтоплене золото, ллється на землю блискучий світ сонця; на ланах грає сонячна хвиля; під хвилею '+
      'спіє хліборобська доля... Легенький вітрець подихає з теплого краю, перебігає з нивки на нивку, живить, '+
      'освіжає кожну билинку... І ведуть вони між собою тиху-таємну розмову: чутно тільки шелест жита, травиці... А '+
      'згори лине жайворонкова пісня: доноситься голос, як срібний дзвіночок,'#$00A0'— тремтить, переливається, застигає в '+
      'повітрі... Перериває його перепелячий крик, зірвавшись угору; заглушає докучне сюрчання трав''яних коників, що '+
      'як не розірвуться,'#$00A0'— і все те зливається докупи в якийсь чудний гомін, вривається в душу, розбуркує в ній '+
      'добрість, щирість, любов до всього... Гарно тобі, любо, весело! На серці стихають негоди, на думку не лізуть '+
      'клопоти: добра надія обгортає тебе добрими думками, бажаннями... Хочеться самому жити й любити; бажаєш кожному '+
      'щастя. Недаром в таку годину'#$00A0'— аби неділя або яке свято — хлібороби виходять на поле хліба обдивлятись!'+
    // Double line break
    #13#10 + #13#10 +
      'Отакої саме пори, в неділю, після раннього обіднього часу,'#$00A0'— тим шляхом, що, звившись гадюкою, пославсь од '+
      'великого села Пісок аж до славного колись Ромодану,'#$00A0'— йшов молодий чоловік. "Не багатого роду!"'#$00A0'— казала '+
      'проста свита, накинута наопашки,'#$00A0'— "та чепурної вдачі",'#$00A0'— одмовляла чиста, біла, на грудях вишивана сорочка, '+
      'виглядаючи з-під свити. Червоний з китицями пояс теліпався до колін, а висока сива шапка з решетилівських '+
      'смушків, перехиляючись набакир, натякала про парубоцьку вдачу...';

  sLoremIpsum = // Nothing fancy with this one
    'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin id sem sed lectus luctus dictum quis a lacus. '+
    'Phasellus eget gravida nulla. Curabitur lacinia leo ut magna tempus, a fermentum metus vestibulum. Mauris leo '+
    'magna, luctus quis rhoncus a, dictum eget mi. Donec et eleifend nulla. Nam ac tempus neque, nec malesuada ante. '+
    'Nam ut accumsan mauris. Pellentesque accumsan erat eu neque mattis, ut congue lectus egestas. Donec ante lorem, '+
    'consequat nec tellus ac, sollicitudin aliquam ipsum. Proin ut purus fermentum, dignissim diam eu, consequat '+
    'ipsum. Suspendisse scelerisque gravida risus, dignissim imperdiet diam rutrum a. Proin nec consectetur neque.'+
    #13+
    'Quisque auctor egestas arcu, non tempor tellus accumsan dignissim. Praesent egestas metus vel erat tincidunt '+
    'tincidunt. Aliquam erat volutpat. Aliquam efficitur elit sed erat fringilla, vel maximus massa sodales. Maecenas '+
    'pharetra metus nisi, nec cursus nunc pellentesque eget. In eget dolor pretium, iaculis felis sed, pulvinar '+
    'metus. Donec placerat pulvinar facilisis. Phasellus mattis elit turpis, bibendum finibus velit lobortis in. '+
    'Donec leo velit, euismod sit amet nibh sed, vehicula scelerisque urna.'+
    #13+
    'Donec sodales diam quis ligula fermentum tincidunt. Morbi accumsan a ligula in fermentum. Maecenas iaculis nec '+
    'sem id malesuada. Curabitur tincidunt a justo eu rutrum. Mauris ullamcorper justo ultricies ipsum aliquet, '+
    'eget sollicitudin nibh suscipit. Etiam auctor sodales est, ut convallis neque elementum vel. Vestibulum volutpat '+
    'dictum nisl, scelerisque maximus est blandit non. Aliquam imperdiet feugiat risus sed ornare. Pellentesque quis '+
    'nibh vitae augue mattis egestas. Etiam rutrum purus non orci volutpat sollicitudin. Donec sit amet est volutpat, '+
    'tempus libero nec, consectetur lorem.';

const
  sExamples: array[0..4] of record
    Text: string;
    LineBreakIsParagraph: boolean;
    DoubleLineBreakIsParagraph: boolean;
  end = (
    (Text: sLoremIpsum; LineBreakIsParagraph: True; DoubleLineBreakIsParagraph: False),
    (Text: sMobyDick; LineBreakIsParagraph: False; DoubleLineBreakIsParagraph: True),
    (Text: sDonQuixote; LineBreakIsParagraph: False; DoubleLineBreakIsParagraph: True),
    (Text: sMaBohème; LineBreakIsParagraph: False; DoubleLineBreakIsParagraph: True),
    (Text: sDoOxenLowWhenMangersAreFull; LineBreakIsParagraph: False; DoubleLineBreakIsParagraph: True)
  );

const
  InnerMargin = 10;

procedure TMainForm.FormCreate(Sender: TObject);
var
  Brush: TSolidBrush;
begin
  Img.SetupBitmap(True, clWhite32);
  Img.Bitmap.Font.Name := 'Georgia';
  Img.Bitmap.Font.Size := 9;
  FontDialog.Font.Assign(Img.Bitmap.Font);

  DisplayFontInfo;

  FCanvas := TCanvas32.Create(Img.Bitmap);
  FCanvas.BeginLockUpdate; // Never unlocked; We draw manually
  Brush := TSolidBrush(FCanvas.Brushes.Add(TSolidBrush));
  Brush.FillColor := clBlack32;
  Brush.FillMode := pfNonZero;

  PaintBox32.BufferOversize := 0;
  PaintBox32.Buffer.Clear(clWhite32);

  FTextLayout := DefaultTextLayout;

  FTextLayout.SingleLine := False;
  FTextLayout.WordWrap := True;

  CheckBoxSingleLine.Checked := FTextLayout.SingleLine;
  CheckBoxWordbreak.Checked := FTextLayout.WordWrap;
  CheckBoxKerning.Checked := FTextLayout.Kerning;
  CheckBoxClipLayout.Checked := FTextLayout.ClipLayout;
  CheckBoxClipRaster.Checked := FTextLayout.ClipRaster;
  TrackBarGamma.Position := Round(GAMMA_VALUE * TrackBarGamma.Tag);
  TrackBarInterWordMax.Position := Round(FTextLayout.MaxInterWordSpaceFactor * TrackBarInterWordMax.Tag);
  TrackBarInterChar.Position := Round(FTextLayout.MaxInterCharSpaceFactor * TrackBarInterChar.Tag);

{$ifndef FPC}

  // What the hell is the point of Lazarus aping Delphi and then refusing to stay compatible?
  // Anyway, the PositionToolTip property is called ScalePos in Lazarus and on top of that
  // it's not implemented for Windows. Good job!
  TrackBarGamma.PositionToolTip := ptTop;
  TrackBarInterWordMax.PositionToolTip := ptTop;
  TrackBarInterChar.PositionToolTip := ptTop;
  TrackBarZoom.PositionToolTip := ptTop;

{$endif}

  // Lazarus resets various properties so we have to set them in code
  StatusBar.SimplePanel := True;
  ButtonAlignHorLeft.Down := True;
  ButtonAlignVerTop.Down := True;

  StatusBar.SimpleText := '';

{$ifndef FPC}
  Self.Padding.SetBounds(4,4,4,4);
  ButtonExit.AlignWithMargins := True;
  ButtonSelectFont.AlignWithMargins := True;
  GroupBoxFont.AlignWithMargins := True;
  GroupBoxLayout.AlignWithMargins := True;
  GroupBoxAlignHorizontal.AlignWithMargins := True;
  GroupBoxAlignVertical.AlignWithMargins := True;
  CheckBoxSingleLine.AlignWithMargins := True;
  CheckBoxWordbreak.AlignWithMargins := True;
  CheckBoxKerning.AlignWithMargins := True;
  CheckBoxClipRaster.AlignWithMargins := True;
  CheckBoxClipLayout.AlignWithMargins := True;
  GroupBoxRendering.AlignWithMargins := True;
  GroupBoxClipping.AlignWithMargins := True;
  GroupBoxGamma.AlignWithMargins := True;
  RadioGroupMethod.AlignWithMargins := True;
  GroupBoxJustification.AlignWithMargins := True;
{$else}
  ButtonExit.BorderSpacing.Around := 4;
  ButtonSelectFont.BorderSpacing.Around := 4;
  GroupBoxFont.BorderSpacing.Around := 4;
  GroupBoxLayout.BorderSpacing.Around := 4;
  GroupBoxAlignHorizontal.BorderSpacing.Around := 4;
  GroupBoxAlignVertical.BorderSpacing.Around := 4;
  CheckBoxSingleLine.BorderSpacing.Around := 4;
  CheckBoxWordbreak.BorderSpacing.Around := 4;
  CheckBoxKerning.BorderSpacing.Around := 4;
  CheckBoxClipRaster.BorderSpacing.Around := 4;
  CheckBoxClipLayout.BorderSpacing.Around := 4;
  GroupBoxRendering.BorderSpacing.Around := 4;
  GroupBoxClipping.BorderSpacing.Around := 4;
  GroupBoxGamma.BorderSpacing.Around := 4;
  RadioGroupMethod.BorderSpacing.Around := 4;
  GroupBoxJustification.BorderSpacing.Around := 4;
{$endif}

  // Advanced typography requires ITextToPathSupport2
  if (not Supports(Img.Bitmap.Backend, ITextToPathSupport2)) then
  begin
    GroupBoxJustification.Hint := 'The current backend does not support the required features';
    GroupBoxJustification.Enabled := False;
    TrackBarInterChar.Enabled := False;
    TrackBarInterWordMax.Enabled := False;
  end;

  FApplyOptions := True;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FCanvas.Free;
end;

procedure TMainForm.ButtonSelectFontClick(Sender: TObject);
begin
  if FontDialog.Execute then
  begin
    Img.Bitmap.Font.Assign(FontDialog.Font);
    DoLayoutAndRender(nil);
    DisplayFontInfo;
  end;
end;

procedure TMainForm.DoRender(Sender: TObject);
begin
  if (not FApplyOptions) then
    exit;

  RenderText;

  StatusBar.SimpleText := Format('Layout: %.0n mS.  Render: %.0n mS', [FLayoutTime * 1.0, FRenderTime * 1.0]);
end;

procedure TMainForm.DoLayoutAndRender(Sender: TObject);
begin
  if (not FApplyOptions) then
    exit;

  BuildPolygonFromText;
  DoRender(nil);
end;

procedure TMainForm.ImgClick(Sender: TObject);
begin
  RadioGroupMethod.ItemIndex := (RadioGroupMethod.ItemIndex + 1) mod RadioGroupMethod.Items.Count;
end;

procedure TMainForm.ImgMouseLeave(Sender: TObject);
begin
  DrawZoom(0, 0);
end;

procedure TMainForm.ImgMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer; Layer: TCustomLayer);
begin
  DrawZoom(X, Y);
end;

procedure TMainForm.BuildPolygonFromText;
var
  DestRect: TFloatRect;
  StopWatch: TStopWatch;
begin
  if (ComboBoxExample.ItemIndex = -1) then
    exit;

  DestRect := FloatRect(Img.BoundsRect);
  GR32.InflateRect(DestRect, -InnerMargin, -InnerMargin);

  FTextLayout.InterLineFactor := 1.2;
  FTextLayout.InterParagraphFactor := 1.6;
  FTextLayout.SingleLine := CheckBoxSingleLine.Checked;
  FTextLayout.WordWrap := CheckBoxWordbreak.Checked;
  FTextLayout.Kerning := CheckBoxKerning.Checked;
  FTextLayout.ClipLayout := CheckBoxClipLayout.Checked;
  FTextLayout.LineBreakIsParagraph := sExamples[ComboBoxExample.ItemIndex].LineBreakIsParagraph;
  FTextLayout.DoubleLineBreakIsParagraph := sExamples[ComboBoxExample.ItemIndex].DoubleLineBreakIsParagraph;

  FCanvas.Clear;

  StopWatch := TStopWatch.StartNew;

  // FCanvas.RenderText(DestRect, sLoremIpsum, FTextLayout);
  FCanvas.RenderText(DestRect, sExamples[ComboBoxExample.ItemIndex].Text, FTextLayout);

  FLayoutTime := StopWatch.ElapsedMilliseconds;
end;

type
  TCanvas32Cracker = class(TCanvas32);

procedure TMainForm.RenderText;
var
  r: TRect;
  StopWatch: TStopWatch;
begin
  Img.SetupBitmap(True, clWhite32);

  r := Img.Bitmap.BoundsRect;
  GR32.InflateRect(r, -InnerMargin, -InnerMargin);
  Img.Bitmap.FrameRectS(r, $FFD0E0FF);

  // Setup clipping so fonts with swashes don't exceed the text area
  FTextLayout.ClipRaster := CheckBoxClipRaster.Checked;
  if (FTextLayout.ClipRaster) then
    Img.Bitmap.ClipRect := r
  else
    Img.Bitmap.ClipRect := Img.Bitmap.BoundsRect;

  StopWatch := TStopWatch.StartNew;

  TCanvas32Cracker(FCanvas).DrawPath(FCanvas);

  FRenderTime := StopWatch.ElapsedMilliseconds;

  DrawZoom(0, 0);
end;

procedure TMainForm.ButtonAlignVerClick(Sender: TObject);
begin
  if (not FApplyOptions) then
    exit;

  FTextLayout.AlignmentVertical := TTextAlignmentVertical(TControl(Sender).Tag);

  DoLayoutAndRender(nil);
end;

procedure TMainForm.ButtonAlignHorClick(Sender: TObject);
begin

  if (FApplyOptions) then
  begin
    FTextLayout.AlignmentHorizontal := TTextAlignmentHorizontal(TControl(Sender).Tag);
    DoLayoutAndRender(nil);
  end;

  TrackBarInterChar.Enabled := GroupBoxJustification.Enabled and (FTextLayout.AlignmentHorizontal = TextAlignHorJustify);
  TrackBarInterWordMax.Enabled := GroupBoxJustification.Enabled and (FTextLayout.AlignmentHorizontal = TextAlignHorJustify);
end;

function FontStylesToString(FontStyles: TFontStyles): string;
var
  Styles: TFontStyles;
begin
  Styles := [fsBold, fsItalic] * FontStyles;
  if Styles = [] then
    Result := ''
  else
  if Styles = [fsBold] then
    Result := ', Bold'
  else
  if Styles = [fsItalic] then
    Result := ', Italic'
  else
    Result := ', Bold & Italic';
end;

procedure TMainForm.DisplayFontInfo;
begin
  LblFontInfo.Caption := Format('%s'#10'%d px%s', [FontDialog.Font.Name, FontDialog.Font.Size, FontStylesToString(FontDialog.Font.Style)]);
end;

procedure TMainForm.DrawZoom(X, Y: integer);
var
  Zoom: Single;
  SrcRect: TRect;
  SrcW, SrcH: integer;
begin
  PaintBox32.Buffer.Clear(clWhite32);

  Zoom := TrackBarZoom.Tag / TrackBarZoom.Position;

  SrcW := Round(PaintBox32.Width * Zoom);
  SrcH := Round(PaintBox32.Height * Zoom);

  SrcRect := MakeRect(0, 0, SrcW, SrcH);
  X := EnsureRange(X - SrcW div 2, 0, Img.Bitmap.Width-SrcW);
  Y := EnsureRange(Y - SrcW div 2, 0, Img.Bitmap.Height-SrcH);

  GR32.OffsetRect(SrcRect, X, Y);

  PaintBox32.Buffer.Draw(PaintBox32.BoundsRect, SrcRect, Img.Bitmap);
  PaintBox32.Repaint;
end;

procedure TMainForm.RadioGroupMethodClick(Sender: TObject);
begin
  case RadioGroupMethod.ItemIndex of
    0: FCanvas.Renderer := TPolygonRenderer32VPR.Create(FCanvas.Bitmap, pfNonZero);
    1: FCanvas.Renderer := TPolygonRenderer32LCD.Create(FCanvas.Bitmap, pfNonZero);
    2: FCanvas.Renderer := TPolygonRenderer32LCD2.Create(FCanvas.Bitmap, pfNonZero);
  end;

  DoRender(nil);
end;

procedure TMainForm.TrackBarGammaChange(Sender: TObject);
var
  Value: Single;
begin
  StatusBar.SimpleText := 'Gamma correction is currently not implemented for text rendering';

  Value := TTrackBar(Sender).Position / TTrackBar(Sender).Tag;

  SetGamma(Value);

  TTrackBar(Sender).Hint := Format('%.2n', [GAMMA_VALUE]);

  DoRender(nil);
end;

procedure TMainForm.TrackBarInterCharChange(Sender: TObject);
var
  Value: Single;
begin
  Value := TTrackBar(Sender).Position / TTrackBar(Sender).Tag;
  TTrackBar(Sender).Hint := Format('%.2n', [Value]);

  if (not FApplyOptions) then
    exit;

  FTextLayout.MaxInterCharSpaceFactor := Value;

  DoLayoutAndRender(nil);
end;

procedure TMainForm.TrackBarInterWordMaxChange(Sender: TObject);
var
  Value: Single;
begin
  Value := TTrackBar(Sender).Position / TTrackBar(Sender).Tag;
  TTrackBar(Sender).Hint := Format('%.2n', [Value]);

  if (not FApplyOptions) then
    exit;

  FTextLayout.MaxInterWordSpaceFactor := Value;

  DoLayoutAndRender(nil);
end;

procedure TMainForm.TrackBarZoomChange(Sender: TObject);
begin
  DrawZoom(0, 0);
end;

procedure TMainForm.ButtonExitClick(Sender: TObject);
begin
  Close;
end;

{ TTrackBar }
{$ifdef WINDOWS}
{$ifdef FPC}
type
  PNMTTDispInfo = PNMTTDispInfoW;

const
  TTN_NEEDTEXT = TTN_NEEDTEXTW;
{$endif}

procedure TTrackBar.WndProc(var Message: TMessage);
var
{$ifdef FPC}
  s: UnicodeString;
{$else}
  s: string;
{$endif}
  NMTTDispInfo: PNMTTDispInfo;
begin
  inherited;

  if (Tag = 0) or (Message.Msg <> WM_NOTIFY) then
    exit;

  NMTTDispInfo := PNMTTDispInfo(Message.LParam);

  // Cast to cardinal for Lazarus :-/
  if Cardinal(NMTTDispInfo.hdr.code) <> Cardinal(TTN_NEEDTEXT) then
    exit;

  s := Format('%.2n', [Position / Tag]);

  StrLCopy(@NMTTDispInfo.szText[0], PWideChar(s), 79);
end;

{$ifdef FPC}
procedure TTrackBar.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or TBS_TOOLTIPS;
end;

procedure TTrackBar.CreateWnd;
begin
  inherited;

//  UpdateWindowStyle(Handle, TBS_TOOLTIPS, TBS_TOOLTIPS);
  SendMessage(Handle, TBM_SETTIPSIDE, TBTS_TOP, 0);
end;
{$endif}

{$endif}

end.
