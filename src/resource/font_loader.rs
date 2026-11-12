use std::cmp::max;
use std::path::PathBuf;

use image::{DynamicImage, Rgba};
use nalgebra::{Vector2, Vector4};
use rusttype::{Font, PositionedGlyph, Scale, point};

use crate::scene::font::{self, FontDataCreateInfo};
pub use crate::scene::font::{KOREAN_FONT_CELL_SIZE, KOREAN_FONT_RENDER_SIZE};
use crate::utilities::system;

/// Standard special symbols useful in game and UI contexts (29 symbols, excluding '~' which is in ASCII 33..=126)
pub const SPECIAL_SYMBOLS: [char; 29] = [
    '·', '…', '‘', '’', '“', '”', '₩', '★', '☆', '▶', '◀', '▲', '▼', '※', '℃', '±', '×', '÷', '°', '○', '●', '◇', '◆',
    '□', '■', '♪', '♬', '♥', '♡',
];

/// Common modern Korean loanwords, interjections, and game terms not in KS X 1001 (45 syllables)
pub const EXTRA_HANGUL_SYLLABLES: &str =
    "똠뷐븨샾솦숔숖쉪슾싴쌒쎠쎅쎼쏫쓔씅옝왭읻쬬쿜퀰퀱퀲퀳킈킦킧턔팦퍠퍤퍨퍰퍱퍳퍵퍾퍿펲퐆픠핰힣";

/// Standard KS X 1001 Modern Hangul Syllables (2,350 syllables)
pub const KS_X_1001_HANGUL: &str = concat!(
    "가각간갇갈갉갊감갑값갓갔강갖갗같갚갛개객갠갤갬갭갯갰갱갸갹갼걀걋걍걔걘걜거걱건걷걸걺검겁것겄겅겆겉겊겋게겐겔겜겝겟겠겡겨격겪견겯결겸겹겻겼경곁계곈곌곕곗고곡곤곧",
    "골곪곬곯곰곱곳공곶과곽관괄괆괌괍괏광괘괜괠괩괬괭괴괵괸괼굄굅굇굉교굔굘굡굣구국군굳굴굵굶굻굼굽굿궁궂궈궉권궐궜궝궤궷귀귁귄귈귐귑귓규균귤그극근귿글긁금급긋긍긔기",
    "긱긴긷길긺김깁깃깅깆깊까깍깎깐깔깖깜깝깟깠깡깥깨깩깬깰깸깹깻깼깽꺄꺅꺌꺼꺽꺾껀껄껌껍껏껐껑께껙껜껨껫껭껴껸껼꼇꼈꼍꼐꼬꼭꼰꼲꼴꼼꼽꼿꽁꽂꽃꽈꽉꽐꽜꽝꽤꽥꽹꾀꾄꾈",
    "꾐꾑꾕꾜꾸꾹꾼꿀꿇꿈꿉꿋꿍꿎꿔꿜꿨꿩꿰꿱꿴꿸뀀뀁뀄뀌뀐뀔뀜뀝뀨끄끅끈끊끌끎끓끔끕끗끙끝끼끽낀낄낌낍낏낑나낙낚난낟날낡낢남납낫났낭낮낯낱낳내낵낸낼냄냅냇냈냉냐냑냔",
    "냘냠냥너넉넋넌널넒넓넘넙넛넜넝넣네넥넨넬넴넵넷넸넹녀녁년녈념녑녔녕녘녜녠노녹논놀놂놈놉놋농높놓놔놘놜놨뇌뇐뇔뇜뇝뇟뇨뇩뇬뇰뇹뇻뇽누눅눈눋눌눔눕눗눙눠눴눼뉘뉜뉠뉨",
    "뉩뉴뉵뉼늄늅늉느늑는늘늙늚늠늡늣능늦늪늬늰늴니닉닌닐닒님닙닛닝닢다닥닦단닫달닭닮닯닳담답닷닸당닺닻닿대댁댄댈댐댑댓댔댕댜더덕덖던덛덜덞덟덤덥덧덩덫덮데덱덴델뎀뎁",
    "뎃뎄뎅뎌뎐뎔뎠뎡뎨뎬도독돈돋돌돎돐돔돕돗동돛돝돠돤돨돼됐되된될됨됩됫됴두둑둔둘둠둡둣둥둬뒀뒈뒝뒤뒨뒬뒵뒷뒹듀듄듈듐듕드득든듣들듦듬듭듯등듸디딕딘딛딜딤딥딧딨딩딪",
    "따딱딴딸땀땁땃땄땅땋때땍땐땔땜땝땟땠땡떠떡떤떨떪떫떰떱떳떴떵떻떼떽뗀뗄뗌뗍뗏뗐뗑뗘뗬또똑똔똘똥똬똴뙈뙤뙨뚜뚝뚠뚤뚫뚬뚱뛔뛰뛴뛸뜀뜁뜅뜨뜩뜬뜯뜰뜸뜹뜻띄띈띌띔띕띠",
    "띤띨띰띱띳띵라락란랄람랍랏랐랑랒랖랗래랙랜랠램랩랫랬랭랴략랸럇량러럭런럴럼럽럿렀렁렇레렉렌렐렘렙렛렝려력련렬렴렵렷렸령례롄롑롓로록론롤롬롭롯롱롸롼뢍뢨뢰뢴뢸룀룁",
    "룃룅료룐룔룝룟룡루룩룬룰룸룹룻룽뤄뤘뤠뤼뤽륀륄륌륏륑류륙륜률륨륩륫륭르륵른를름릅릇릉릊릍릎리릭린릴림립릿링마막만많맏말맑맒맘맙맛망맞맡맣매맥맨맬맴맵맷맸맹맺먀먁",
    "먈먕머먹먼멀멂멈멉멋멍멎멓메멕멘멜멤멥멧멨멩며멱면멸몃몄명몇몌모목몫몬몰몲몸몹못몽뫄뫈뫘뫙뫼묀묄묍묏묑묘묜묠묩묫무묵묶문묻물묽묾뭄뭅뭇뭉뭍뭏뭐뭔뭘뭡뭣뭬뮈뮌뮐뮤",
    "뮨뮬뮴뮷므믄믈믐믓미믹민믿밀밂밈밉밋밌밍및밑바박밖밗반받발밝밞밟밤밥밧방밭배백밴밸뱀뱁뱃뱄뱅뱉뱌뱍뱐뱝버벅번벋벌벎범법벗벙벚베벡벤벧벨벰벱벳벴벵벼벽변별볍볏볐병",
    "볕볘볜보복볶본볼봄봅봇봉봐봔봤봬뵀뵈뵉뵌뵐뵘뵙뵤뵨부북분붇불붉붊붐붑붓붕붙붚붜붤붰붸뷔뷕뷘뷜뷩뷰뷴뷸븀븃븅브븍븐블븜븝븟비빅빈빌빎빔빕빗빙빚빛빠빡빤빨빪빰빱빳빴",
    "빵빻빼빽뺀뺄뺌뺍뺏뺐뺑뺘뺙뺨뻐뻑뻔뻗뻘뻠뻣뻤뻥뻬뼁뼈뼉뼘뼙뼛뼜뼝뽀뽁뽄뽈뽐뽑뽕뾔뾰뿅뿌뿍뿐뿔뿜뿟뿡쀼쁑쁘쁜쁠쁨쁩삐삑삔삘삠삡삣삥사삭삯산삳살삵삶삼삽삿샀상샅새색",
    "샌샐샘샙샛샜생샤샥샨샬샴샵샷샹섀섄섈섐섕서석섞섟선섣설섦섧섬섭섯섰성섶세섹센셀셈셉셋셌셍셔셕션셜셤셥셧셨셩셰셴셸솅소속솎손솔솖솜솝솟송솥솨솩솬솰솽쇄쇈쇌쇔쇗쇘쇠",
    "쇤쇨쇰쇱쇳쇼쇽숀숄숌숍숏숑수숙순숟술숨숩숫숭숯숱숲숴쉈쉐쉑쉔쉘쉠쉥쉬쉭쉰쉴쉼쉽쉿슁슈슉슐슘슛슝스슥슨슬슭슴습슷승시식신싣실싫심십싯싱싶싸싹싻싼쌀쌈쌉쌌쌍쌓쌔쌕쌘",
    "쌜쌤쌥쌨쌩썅써썩썬썰썲썸썹썼썽쎄쎈쎌쏀쏘쏙쏜쏟쏠쏢쏨쏩쏭쏴쏵쏸쐈쐐쐤쐬쐰쐴쐼쐽쑈쑤쑥쑨쑬쑴쑵쑹쒀쒔쒜쒸쒼쓩쓰쓱쓴쓸쓺쓿씀씁씌씐씔씜씨씩씬씰씸씹씻씽아악안앉않알앍",
    "앎앓암압앗았앙앝앞애액앤앨앰앱앳앴앵야약얀얄얇얌얍얏양얕얗얘얜얠얩어억언얹얻얼얽얾엄업없엇었엉엊엌엎에엑엔엘엠엡엣엥여역엮연열엶엷염엽엾엿였영옅옆옇예옌옐옘옙옛",
    "옜오옥온올옭옮옰옳옴옵옷옹옻와왁완왈왐왑왓왔왕왜왝왠왬왯왱외왹왼욀욈욉욋욍요욕욘욜욤욥욧용우욱운울욹욺움웁웃웅워웍원월웜웝웠웡웨웩웬웰웸웹웽위윅윈윌윔윕윗윙유육",
    "윤율윰윱윳융윷으윽은을읊음읍읏응읒읓읔읕읖읗의읜읠읨읫이익인일읽읾잃임입잇있잉잊잎자작잔잖잗잘잚잠잡잣잤장잦재잭잰잴잼잽잿쟀쟁쟈쟉쟌쟎쟐쟘쟝쟤쟨쟬저적전절젊점접",
    "젓정젖제젝젠젤젬젭젯젱져젼졀졈졉졌졍졔조족존졸졺좀좁좃종좆좇좋좌좍좔좝좟좡좨좼좽죄죈죌죔죕죗죙죠죡죤죵주죽준줄줅줆줌줍줏중줘줬줴쥐쥑쥔쥘쥠쥡쥣쥬쥰쥴쥼즈즉즌즐즘",
    "즙즛증지직진짇질짊짐집짓징짖짙짚짜짝짠짢짤짧짬짭짯짰짱째짹짼쨀쨈쨉쨋쨌쨍쨔쨘쨩쩌쩍쩐쩔쩜쩝쩟쩠쩡쩨쩽쪄쪘쪼쪽쫀쫄쫌쫍쫏쫑쫓쫘쫙쫠쫬쫴쬈쬐쬔쬘쬠쬡쭁쭈쭉쭌쭐쭘쭙쭝",
    "쭤쭸쭹쮜쮸쯔쯤쯧쯩찌찍찐찔찜찝찡찢찧차착찬찮찰참찹찻찼창찾채책챈챌챔챕챗챘챙챠챤챦챨챰챵처척천철첨첩첫첬청체첵첸첼쳄쳅쳇쳉쳐쳔쳤쳬쳰촁초촉촌촐촘촙촛총촤촨촬촹최",
    "쵠쵤쵬쵭쵯쵱쵸춈추축춘출춤춥춧충춰췄췌췐취췬췰췸췹췻췽츄츈츌츔츙츠측츤츨츰츱츳층치칙친칟칠칡침칩칫칭카칵칸칼캄캅캇캉캐캑캔캘캠캡캣캤캥캬캭컁커컥컨컫컬컴컵컷컸컹",
    "케켁켄켈켐켑켓켕켜켠켤켬켭켯켰켱켸코콕콘콜콤콥콧콩콰콱콴콸쾀쾅쾌쾡쾨쾰쿄쿠쿡쿤쿨쿰쿱쿳쿵쿼퀀퀄퀑퀘퀭퀴퀵퀸퀼큄큅큇큉큐큔큘큠크큭큰클큼큽킁키킥킨킬킴킵킷킹타탁탄",
    "탈탉탐탑탓탔탕태택탠탤탬탭탯탰탱탸턍터턱턴털턺텀텁텃텄텅테텍텐텔템텝텟텡텨텬텼톄톈토톡톤톨톰톱톳통톺톼퇀퇘퇴퇸툇툉툐투툭툰툴툼툽툿퉁퉈퉜퉤튀튁튄튈튐튑튕튜튠튤튬",
    "튱트특튼튿틀틂틈틉틋틔틘틜틤틥티틱틴틸팀팁팃팅파팍팎판팔팖팜팝팟팠팡팥패팩팬팰팸팹팻팼팽퍄퍅퍼퍽펀펄펌펍펏펐펑페펙펜펠펨펩펫펭펴편펼폄폅폈평폐폘폡폣포폭폰폴폼폽",
    "폿퐁퐈퐝푀푄표푠푤푭푯푸푹푼푿풀풂품풉풋풍풔풩퓌퓐퓔퓜퓟퓨퓬퓰퓸퓻퓽프픈플픔픕픗피픽핀필핌핍핏핑하학한할핥함합핫항해핵핸핼햄햅햇했행햐향허헉헌헐헒험헙헛헝헤헥헨",
    "헬헴헵헷헹혀혁현혈혐협혓혔형혜혠혤혭호혹혼홀홅홈홉홋홍홑화확환활홧황홰홱홴횃횅회획횐횔횝횟횡효횬횰횹횻후훅훈훌훑훔훗훙훠훤훨훰훵훼훽휀휄휑휘휙휜휠휨휩휫휭휴휵휸",
    "휼흄흇흉흐흑흔흖흗흘흙흠흡흣흥흩희흰흴흼흽힁히힉힌힐힘힙힛힝",
);

/// Builds an optimized character set for Korean font atlas generation based on KS X 1001 + essentials:
/// - Index 0: Space / fallback slot (' ')
/// - ASCII 33..=126 (94 characters)
/// - Hangul Compatibility Jamo 0x3131..=0x3163 (51 characters)
/// - Common Special Symbols (29 characters)
/// - KS X 1001 Modern Hangul Syllables (2,350 characters)
/// - Extra modern Korean syllables (45 characters)
/// Total: 2,570 characters (fits within a 51x51 grid with 64x64 cell size, yielding 3264x3264 atlas)
pub fn build_korean_character_set() -> String {
    build_korean_character_set_with_extra("")
}

pub fn build_korean_character_set_with_extra(extra_text: &str) -> String {
    let mut chars = String::with_capacity(3000);
    chars.push(' ');

    for cp in 33u32..=126u32 {
        if let Some(ch) = char::from_u32(cp) {
            chars.push(ch);
        }
    }

    for cp in 0x3131u32..=0x3163u32 {
        if let Some(ch) = char::from_u32(cp) {
            chars.push(ch);
        }
    }

    for sym in SPECIAL_SYMBOLS.iter() {
        chars.push(*sym);
    }

    for ch in KS_X_1001_HANGUL.chars() {
        chars.push(ch);
    }

    for ch in EXTRA_HANGUL_SYLLABLES.chars() {
        chars.push(ch);
    }

    if !extra_text.is_empty() {
        for ch in extra_text.chars() {
            if !chars.contains(ch) {
                chars.push(ch);
            }
        }
    }

    chars
}

/// 1D Felzenszwalb-Huttenlocher Distance Transform (parabolic envelope)
fn edt_1d(f: &[f32], d: &mut [f32], n: usize, v: &mut [usize], z: &mut [f32]) {
    let mut k = 0usize;
    v[0] = 0;
    z[0] = -1e20;
    z[1] = 1e20;

    for q in 1..n {
        let mut s = ((f[q] + (q * q) as f32) - (f[v[k]] + (v[k] * v[k]) as f32)) / (2 * q - 2 * v[k]) as f32;
        while s <= z[k] {
            k -= 1;
            s = ((f[q] + (q * q) as f32) - (f[v[k]] + (v[k] * v[k]) as f32)) / (2 * q - 2 * v[k]) as f32;
        }
        k += 1;
        v[k] = q;
        z[k] = s;
        z[k + 1] = 1e20;
    }

    k = 0;
    for q in 0..n {
        while z[k + 1] < q as f32 {
            k += 1;
        }
        let dx = (q as isize - v[k] as isize) as f32;
        d[q] = dx * dx + f[v[k]];
    }
}

/// Computes 2D Euclidean Distance Transform for a single rectangular cell
fn compute_cell_edt(
    image: &image::RgbaImage,
    offset_x: u32,
    offset_y: u32,
    cell_w: u32,
    cell_h: u32,
    max_dist: f32,
    distance_field_image: &mut image::RgbaImage,
    v_buf: &mut [usize],
    z_buf: &mut [f32],
    f_grid: &mut [f32],
    d_grid: &mut [f32],
) {
    let mut has_fg = false;
    let cw = cell_w as usize;
    let ch = cell_h as usize;

    for py in 0..cell_h {
        for px in 0..cell_w {
            let pixel = image.get_pixel(offset_x + px, offset_y + py);
            let idx = py as usize * cw + px as usize;
            if pixel[3] > 128 {
                f_grid[idx] = 0.0;
                has_fg = true;
            } else {
                f_grid[idx] = 1e10;
            }
        }
    }

    if !has_fg {
        for py in 0..cell_h {
            for px in 0..cell_w {
                distance_field_image.put_pixel(offset_x + px, offset_y + py, Rgba([255, 255, 255, 0]));
            }
        }
        return;
    }

    // Pass 1: 1D EDT on each column
    let mut col_in = vec![0.0f32; ch];
    let mut col_out = vec![0.0f32; ch];
    for px in 0..cw {
        for py in 0..ch {
            col_in[py] = f_grid[py * cw + px];
        }
        edt_1d(&col_in, &mut col_out, ch, v_buf, z_buf);
        for py in 0..ch {
            d_grid[py * cw + px] = col_out[py];
        }
    }

    // Pass 2: 1D EDT on each row
    let mut row_in = vec![0.0f32; cw];
    let mut row_out = vec![0.0f32; cw];
    for py in 0..ch {
        for px in 0..cw {
            row_in[px] = d_grid[py * cw + px];
        }
        edt_1d(&row_in, &mut row_out, cw, v_buf, z_buf);
        for px in 0..cw {
            let dist_sq = row_out[px];
            let dist = dist_sq.sqrt();
            let norm_dist = (dist / max_dist).clamp(0.0, 1.0);
            let alpha = 255u8.saturating_sub((norm_dist.sqrt() * 255.0).min(255.0) as u8);
            distance_field_image.put_pixel(offset_x + px as u32, offset_y + py as u32, Rgba([255, 255, 255, alpha]));
        }
    }
}

/// Generates a font atlas texture and metadata from an arbitrary character sequence.
/// Supports true monospace positioning (half-width in central slot, full-width in full cell).
pub fn get_font_data_create_info_from_chars(
    font_source_file: &PathBuf,
    cell_size: u32,
    font_render_size: f32,
    font_data_name: &str,
    font_texture_file_path: &PathBuf,
    chars: &str,
) -> FontDataCreateInfo {
    log::info!("create font texture from chars: {:?}", font_texture_file_path);
    let font_data = system::load(font_source_file);
    let font = Font::try_from_bytes(font_data.get_ref()).expect("Error constructing Font");
    let colour = (255, 255, 255);

    let char_vec: Vec<char> = chars.chars().collect();
    let total_count = char_vec.len() as u32;
    let count_of_side = (total_count as f32).sqrt().ceil() as u32;

    let cell_w = cell_size;
    let cell_h = cell_size;
    let atlas_w = cell_w * count_of_side;
    let atlas_h = cell_h * count_of_side;
    let max_dist = (max(cell_w, cell_h) - 1) as f32;

    let scale = Scale::uniform(font_render_size);
    let v_metrics = font.v_metrics(scale);

    let mut raw_image = DynamicImage::new_rgba8(atlas_w, atlas_h).to_rgba8();

    let inv_atlas_w = 1.0 / atlas_w as f32;
    let inv_atlas_h = 1.0 / atlas_h as f32;
    let em_size = font_render_size;
    let mut glyphs: Vec<font::GlyphMetric> = Vec::with_capacity(total_count as usize);

    for (idx, &ch) in char_vec.iter().enumerate() {
        let grid_x = (idx as u32) % count_of_side;
        let grid_y = (idx as u32) / count_of_side;
        let cell_ox = (grid_x * cell_w) as f32;
        let cell_oy = (grid_y * cell_h) as f32;

        if ch == ' ' {
            let half_advance = 0.5f32;
            glyphs.push(font::GlyphMetric {
                _uv_min: Vector2::new(cell_ox * inv_atlas_w, cell_oy * inv_atlas_h),
                _uv_max: Vector2::new(
                    (cell_ox + cell_w as f32) * inv_atlas_w,
                    (cell_oy + cell_h as f32) * inv_atlas_h,
                ),
                _plane_bounds: Vector4::new(0.0, 0.0, 0.0, 1.0),
                _advance_x: half_advance,
            });
            continue;
        }

        let glyph = font.glyph(ch).scaled(scale);
        let h_metrics = glyph.h_metrics();
        let advance_w = h_metrics.advance_width;
        let is_full = advance_w > font_render_size * 0.7;

        let px = if is_full {
            cell_ox + (cell_w as f32 - advance_w) * 0.5
        } else {
            let half_slot_w = cell_w as f32 * 0.5;
            cell_ox + (cell_w as f32 * 0.25) + (half_slot_w - advance_w) * 0.5
        };

        // Align baseline
        let py = cell_oy + (cell_h as f32 - font_render_size) * 0.5 + v_metrics.ascent;
        let pos_glyph = glyph.positioned(point(px, py));

        let (uv_min, uv_max, plane_bounds, advance_x) = if is_full {
            let u1 = cell_ox * inv_atlas_w;
            let v1 = cell_oy * inv_atlas_h;
            let u2 = (cell_ox + cell_w as f32) * inv_atlas_w;
            let v2 = (cell_oy + cell_h as f32) * inv_atlas_h;

            let norm_w = cell_w as f32 / em_size;
            let norm_h = cell_h as f32 / em_size;
            let norm_advance = 1.0f32;
            let center_x = norm_advance * 0.5;
            let half_norm_w = norm_w * 0.5;
            let x1 = center_x - half_norm_w;
            let x2 = center_x + half_norm_w;
            let y1 = -((cell_h as f32 - font_render_size) * 0.5) / em_size;
            let y2 = y1 + norm_h;

            (
                Vector2::new(u1, v1),
                Vector2::new(u2, v2),
                Vector4::new(x1, y1, x2, y2),
                norm_advance,
            )
        } else {
            let half_slot_w = cell_w as f32 * 0.5;
            let slot_x1 = cell_ox + cell_w as f32 * 0.25;
            let slot_x2 = slot_x1 + half_slot_w;

            let u1 = slot_x1 * inv_atlas_w;
            let v1 = cell_oy * inv_atlas_h;
            let u2 = slot_x2 * inv_atlas_w;
            let v2 = (cell_oy + cell_h as f32) * inv_atlas_h;

            let norm_w = half_slot_w / em_size;
            let norm_h = cell_h as f32 / em_size;
            let norm_advance = 0.5f32;
            let center_x = norm_advance * 0.5;
            let half_norm_w = norm_w * 0.5;
            let x1 = center_x - half_norm_w;
            let x2 = center_x + half_norm_w;
            let y1 = -((cell_h as f32 - font_render_size) * 0.5) / em_size;
            let y2 = y1 + norm_h;

            (
                Vector2::new(u1, v1),
                Vector2::new(u2, v2),
                Vector4::new(x1, y1, x2, y2),
                norm_advance,
            )
        };

        glyphs.push(font::GlyphMetric {
            _uv_min: uv_min,
            _uv_max: uv_max,
            _plane_bounds: plane_bounds,
            _advance_x: advance_x,
        });

        if let Some(bbox) = pos_glyph.pixel_bounding_box() {
            pos_glyph.draw(|gx, gy, v| {
                let img_x = bbox.min.x + gx as i32;
                let img_y = bbox.min.y + gy as i32;
                if img_x >= 0 && img_x < atlas_w as i32 && img_y >= 0 && img_y < atlas_h as i32 {
                    let old_alpha = raw_image.get_pixel(img_x as u32, img_y as u32)[3];
                    let new_alpha = (v * 255.0) as u8;
                    raw_image.put_pixel(
                        img_x as u32,
                        img_y as u32,
                        Rgba([colour.0, colour.1, colour.2, old_alpha.max(new_alpha)]),
                    );
                }
            });
        }
    }

    // Distance Field Computation
    if font::USE_DISTANCE_FIELD {
        let max_dim = max(cell_w, cell_h) as usize + 2;
        let mut v_buf = vec![0usize; max_dim];
        let mut z_buf = vec![0.0f32; max_dim + 1];
        let mut f_grid = vec![0.0f32; (cell_w * cell_h) as usize];
        let mut d_grid = vec![0.0f32; (cell_w * cell_h) as usize];

        let mut distance_field_image = DynamicImage::new_rgba8(atlas_w, atlas_h).to_rgba8();

        for y in 0..count_of_side {
            for x in 0..count_of_side {
                let offset_x = x * cell_w;
                let offset_y = y * cell_h;
                compute_cell_edt(
                    &raw_image,
                    offset_x,
                    offset_y,
                    cell_w,
                    cell_h,
                    max_dist,
                    &mut distance_field_image,
                    &mut v_buf,
                    &mut z_buf,
                    &mut f_grid,
                    &mut d_grid,
                );
            }
        }

        distance_field_image.save(font_texture_file_path).unwrap();
    } else {
        raw_image.save(font_texture_file_path).unwrap();
    }

    FontDataCreateInfo {
        _font_data_name: String::from(font_data_name),
        _range_min: 32,
        _range_max: 126,
        _text_count: total_count,
        _count_of_side: count_of_side,
        _font_size: cell_size as f32,
        _chars: String::from(chars),
        _glyphs: glyphs,
    }
}

/// Legacy / Range-based font atlas generator using high-speed EDT algorithm.
pub fn get_font_data_create_info(
    font_source_file: &PathBuf,
    font_size: f32,
    padding: f32,
    font_data_name: &str,
    font_texture_file_path: &PathBuf,
    range_min: u32,
    range_max: u32,
) -> FontDataCreateInfo {
    log::info!("create font texture: {:?}", font_texture_file_path);
    let font_data = system::load(font_source_file);
    let font = Font::try_from_bytes(font_data.get_ref()).expect("Error constructing Font");
    let colour = (255, 255, 255);
    let scale = Scale::uniform(font_size);
    let v_metrics = font.v_metrics(scale);
    let count = (range_max - range_min) + 1;
    let count_of_side = (count as f32).sqrt().ceil() as u32;
    let glyphs_height = (v_metrics.ascent - v_metrics.descent + padding * 2.0).ceil() as u32;
    let mut glyphs_width = 0;
    let mut glyphs_list: Vec<Vec<PositionedGlyph>> = Vec::new();

    for text in range_min..(range_max + 1) {
        let ch = char::from_u32(text).unwrap_or('?');
        let mut text_str = String::new();
        text_str.push(ch);
        let glyphs: Vec<PositionedGlyph> =
            font.layout(&text_str, scale, point(padding, padding + v_metrics.ascent)).collect();
        let glyphs_first_bounding_box = glyphs.first().and_then(|g| g.pixel_bounding_box());
        let glyphs_last_bounding_box = glyphs.last().and_then(|g| g.pixel_bounding_box());
        match (glyphs_first_bounding_box, glyphs_last_bounding_box) {
            (Some(glyphs_first_bounding_box), Some(glyphs_last_bounding_box)) => {
                let min_x = glyphs_first_bounding_box.min.x;
                let max_x = glyphs_last_bounding_box.max.x;
                glyphs_width = max(glyphs_width, ((max_x - min_x) as f32 + padding * 2.0) as u32);
            }
            _ => {}
        }
        glyphs_list.push(glyphs);
    }

    let mut image = DynamicImage::new_rgba8(glyphs_width * count_of_side, glyphs_height * count_of_side).to_rgba8();
    let mut text_index = range_min;
    for y in 0..count_of_side {
        for x in 0..count_of_side {
            let glyphs = &glyphs_list[(x + y * count_of_side) as usize];
            for glyph in glyphs.iter() {
                if let Some(bounding_box) = glyph.pixel_bounding_box() {
                    glyph.draw(|px, py, v| {
                        let px = px + bounding_box.min.x as u32 + x * glyphs_width;
                        let py = py + bounding_box.min.y as u32 + y * glyphs_height;
                        image.put_pixel(px, py, Rgba([colour.0, colour.1, colour.2, (v * 255.0) as u8]))
                    });
                }
            }

            text_index += 1;
            if range_max < text_index {
                break;
            }
        }
        if range_max < text_index {
            break;
        }
    }

    if font::USE_DISTANCE_FIELD {
        let cell_width = image.width() / count_of_side;
        let cell_height = image.height() / count_of_side;
        let max_dist = (max(cell_width, cell_height) - 1) as f32;
        let mut distance_field_image = DynamicImage::new_rgba8(image.width(), image.height()).to_rgba8();

        let max_dim = max(cell_width, cell_height) as usize + 2;
        let mut v_buf = vec![0usize; max_dim];
        let mut z_buf = vec![0.0f32; max_dim + 1];
        let mut f_grid = vec![0.0f32; (cell_width * cell_height) as usize];
        let mut d_grid = vec![0.0f32; (cell_width * cell_height) as usize];

        for y in 0..count_of_side {
            for x in 0..count_of_side {
                let offset_x = x * cell_width;
                let offset_y = y * cell_height;
                compute_cell_edt(
                    &image,
                    offset_x,
                    offset_y,
                    cell_width,
                    cell_height,
                    max_dist,
                    &mut distance_field_image,
                    &mut v_buf,
                    &mut z_buf,
                    &mut f_grid,
                    &mut d_grid,
                );
            }
        }

        distance_field_image.save(font_texture_file_path).unwrap();
    } else {
        image.save(font_texture_file_path).unwrap();
    }

    let inv_image_w = 1.0 / image.width() as f32;
    let inv_image_h = 1.0 / image.height() as f32;
    let em_size = font_size;
    let cell_w_f = glyphs_width as f32;
    let cell_h_f = glyphs_height as f32;
    let mut glyphs: Vec<font::GlyphMetric> = Vec::with_capacity(count as usize);

    for (idx, _glyph_group) in glyphs_list.iter().enumerate() {
        let grid_x = (idx as u32) % count_of_side;
        let grid_y = (idx as u32) / count_of_side;
        let cell_ox = (grid_x * glyphs_width) as f32;
        let cell_oy = (grid_y * glyphs_height) as f32;

        let u1 = cell_ox * inv_image_w;
        let v1 = cell_oy * inv_image_h;
        let u2 = (cell_ox + cell_w_f) * inv_image_w;
        let v2 = (cell_oy + cell_h_f) * inv_image_h;

        let ch = char::from_u32(range_min + idx as u32).unwrap_or('?');
        let advance_w = font.glyph(ch).scaled(scale).h_metrics().advance_width;
        let norm_advance = advance_w / em_size;

        let norm_w = cell_w_f / em_size;
        let norm_h = cell_h_f / em_size;
        let center_x = norm_advance * 0.5;
        let half_norm_w = norm_w * 0.5;
        let x1 = center_x - half_norm_w;
        let x2 = center_x + half_norm_w;
        let y1 = -(padding / em_size);
        let y2 = y1 + norm_h;

        glyphs.push(font::GlyphMetric {
            _uv_min: Vector2::new(u1, v1),
            _uv_max: Vector2::new(u2, v2),
            _plane_bounds: Vector4::new(x1, y1, x2, y2),
            _advance_x: norm_advance,
        });
    }

    FontDataCreateInfo {
        _font_data_name: String::from(font_data_name),
        _range_min: range_min,
        _range_max: range_max,
        _text_count: range_max - range_min + 1,
        _count_of_side: count_of_side,
        _font_size: font_size,
        _chars: String::new(),
        _glyphs: glyphs,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore = "Offline generator for static font files; DynamicFontAtlas is used at runtime"]
    fn test_regenerate_ubuntu_sans_mono_textures() {
        let root = if PathBuf::from("resources").exists() {
            PathBuf::from(".")
        } else {
            PathBuf::from("..")
        };
        let fonts = [("UbuntuSansMono-Regular", "UbuntuSansMono-Regular_Basic_Latin")];

        for (font_file, font_data_name) in fonts {
            let font_source_path = root.join(format!("resources/engine_resources/externals/fonts/{}.ttf", font_file));
            let font_texture_path = root.join(format!(
                "resources/engine_resources/externals/textures/fonts/{}.png",
                font_data_name
            ));
            let font_file_path = root.join(format!("resources/engine_resources/fonts/{}.font", font_data_name));

            if font_source_path.exists() {
                let create_info = get_font_data_create_info(
                    &font_source_path,
                    font::FONT_SIZE as f32,
                    font::FONT_PADDING as f32,
                    font_data_name,
                    &font_texture_path,
                    32,
                    127,
                );
                assert_eq!(create_info._text_count, 96);
                assert_eq!(create_info._count_of_side, 10);
                assert_eq!(create_info._glyphs.len(), 96);
                assert!(font_texture_path.exists());

                let write_contents = serde_json::to_string(&create_info).expect("Failed to serialize.");
                std::fs::write(&font_file_path, &write_contents).expect("Failed to write font file");

                let engine_font_path =
                    root.join(format!("RustEngine3D/engine_resources/fonts/{}.font", font_data_name));
                if engine_font_path.parent().map_or(false, |p| p.exists()) {
                    let _ = std::fs::write(&engine_font_path, &write_contents);
                }
            }
        }
    }

    #[test]
    #[ignore = "Offline generator for static font files; DynamicFontAtlas is used at runtime"]
    fn test_regenerate_korean_monospace_font_via_font_loader() {
        let root = if PathBuf::from("resources").exists() {
            PathBuf::from(".")
        } else {
            PathBuf::from("..")
        };
        let fonts = [
            ("D2Coding-Regular", "D2Coding-Regular_Korean"),
            ("NanumGothicCoding-Regular", "NanumGothicCoding-Regular_Korean"),
            ("Pretendard-Regular", "Pretendard-Regular_Korean"),
        ];

        let chars = build_korean_character_set();
        for (font_file, font_data_name) in fonts {
            let font_source_path = root.join(format!("resources/engine_resources/externals/fonts/{}.ttf", font_file));
            let font_texture_path = root.join(format!(
                "resources/engine_resources/externals/textures/fonts/{}.png",
                font_data_name
            ));
            let font_file_path = root.join(format!("resources/engine_resources/fonts/{}.font", font_data_name));

            if font_source_path.exists() {
                let create_info = get_font_data_create_info_from_chars(
                    &font_source_path,
                    KOREAN_FONT_CELL_SIZE,
                    KOREAN_FONT_RENDER_SIZE,
                    font_data_name,
                    &font_texture_path,
                    &chars,
                );
                assert_eq!(create_info._font_data_name, font_data_name);
                assert!(create_info._text_count >= 2500);
                assert_eq!(create_info._count_of_side, 51);
                assert_eq!(create_info._glyphs.len(), create_info._text_count as usize);
                assert_eq!(create_info._font_size, KOREAN_FONT_CELL_SIZE as f32);
                assert!(font_texture_path.exists());

                let write_contents = serde_json::to_string(&create_info).expect("Failed to serialize.");
                std::fs::write(&font_file_path, &write_contents).expect("Failed to write font file");

                let engine_font_path =
                    root.join(format!("RustEngine3D/engine_resources/fonts/{}.font", font_data_name));
                if engine_font_path.parent().map_or(false, |p| p.exists()) {
                    let _ = std::fs::write(&engine_font_path, &write_contents);
                }
            }
        }
    }

    #[test]
    fn test_auto_regenerate_missing_font_texture() {
        use crate::resource::resource::EngineResources;
        let font_data_name = "UbuntuSansMono-Regular_Basic_Latin";
        let found = EngineResources::find_font_source_file(font_data_name);
        assert!(found.is_some(), "Must locate font source for UbuntuSansMono-Regular");

        let d2_found = EngineResources::find_font_source_file("D2Coding-Regular_Korean");
        assert!(d2_found.is_some(), "Must locate font source for D2Coding-Regular");

        let test_texture_path = PathBuf::from("/tmp/test_auto_gen_font_texture.png");
        if test_texture_path.exists() {
            let _ = std::fs::remove_file(&test_texture_path);
        }

        let create_info = FontDataCreateInfo {
            _font_data_name: font_data_name.to_string(),
            _range_min: 32,
            _range_max: 127,
            _text_count: 96,
            _count_of_side: 10,
            _font_size: font::FONT_SIZE as f32,
            _chars: String::new(),
            _glyphs: Vec::new(),
        };

        EngineResources::ensure_font_texture(font_data_name, &create_info, &test_texture_path);
        assert!(
            test_texture_path.exists(),
            "ensure_font_texture must auto-generate missing png"
        );
        let _ = std::fs::remove_file(&test_texture_path);

        let test_korean_path = PathBuf::from("/tmp/test_korean_auto_gen.png");
        if test_korean_path.exists() {
            let _ = std::fs::remove_file(&test_korean_path);
        }
        let korean_info = FontDataCreateInfo {
            _font_data_name: "D2Coding-Regular_Korean".to_string(),
            _range_min: 0,
            _range_max: 0,
            _text_count: 5,
            _count_of_side: 3,
            _font_size: 24.0,
            _chars: " 가나다".to_string(),
            _glyphs: Vec::new(),
        };
        EngineResources::ensure_font_texture("D2Coding-Regular_Korean", &korean_info, &test_korean_path);
        assert!(
            test_korean_path.exists(),
            "ensure_font_texture must auto-generate missing Korean png"
        );
        let _ = std::fs::remove_file(&test_korean_path);
    }
}
