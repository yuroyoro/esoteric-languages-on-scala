package com.yuroyoro.esoteric.brainfuck

object NagatoApp extends SampleApp {

  lazy val programName = "プログラミング言語「長門有希」"

  lazy val incTokens   = List("…")
  lazy val decTokens   = List("・")
  lazy val nextTokens  = List("………。")
  lazy val prevTokens  = List("…………。")
  lazy val putTokens   = List("………………。")
  lazy val getTokens   = List("……………。")
  lazy val startTokens = List("「")
  lazy val endTokens   = List("」")

  lazy val helloworld = """
涼宮ハルヒのこと……… 。それと、わたしのこと……… 。あなたに教えておく…… 。

（涼宮とお前が…何だって？）

「うまく言語化できない………。情報の伝達に齟齬が発生するかもしれない………………
…… 。でも、聞いて………。涼宮ハルヒとわたしは普通の人間じゃない……………… 。

（なんとなく普通じゃないのは、わかるけどさ）

そうじゃない…………… 。 性格に普遍的な性質を持っていないという意味ではなく、文
字通りの意味で、彼女とわたしはあなたのような大多数の人間と同じとは言えない………。
この銀河を統括する情報統合思念体によってつくられた対有機生命体コンタクト用ヒュー
マノイドインターフェイス…………… 。それが、わたし…………。

（はい？）

通俗的な用語を使用すると宇宙人に該当する存在…………。

（う、ちゅうじん？）

わたしの仕事は涼宮ハルヒを観察して、入手した情報を統合思念体に報告すること…………。

（えっ？）

生み出されてから3年間、私はずっとそうやって過ごしてきた・・・。この3年間は特別な
不確定要素がなく、いたって平穏。でも最近になって無視出来ないイレギュラー因子が涼
宮ハルヒの周囲に現れた…… 。それが、あなた。」

情報統合思念体にとって銀河の辺境に位置するこの星系の第３惑星に特別な価値などなか
った………。でも現有生命体が地球と呼称するこの惑星で進化した二足歩行動物に知性と
呼ばれる思索能力が芽生えたことにより、その重要度は増大した………………。もしかし
たら自分たちが陥っている自律進化の閉塞状態を打開する可能性があるかも知れなかった
から………。宇宙に偏在する有機生命体に意識が生じるのはありふれた現象だったが、高
次の知性を持つまでに進化した例は地球人類が唯一だった。統合思念体は注意深くかつ綿
密に観測を続けた…… 。

そして3年前………………。惑星表面で他では類を見ない異常な情報フレアを観測した………………… 。
弓状列島の一地域から噴出した情報爆発は瞬く間に惑星全土を覆い、惑星外空間に拡散し
た………………。

その中心にいたのが涼宮ハルヒ………………。
以後3年間、あらゆる角度から涼宮ハルヒという個体に対し調査がなされた……… 。

しかし未だその正体は不明………………。それでも統合思念体の一部は、彼女こそ人類の、
ひいては情報生命体である自分たちに自律進化の切っ掛けを与える存在として涼宮ハルヒ
の存在を解析を行っている………。情報生命体である彼らは有機生命体と直接的にコミュ
ニケートできない・・・。言語を持たないから…… 。 人間は言葉を抜きにして概念を伝
達する術を持たない………………。だからわたしのような人間用のインターフェイスを作
った・・・・・・。情報統合思念体はわたしを通して人間とコンタクト出来る・・・・・・。

涼宮ハルヒは自律進化の可能性を秘めている………………。恐らく彼女には自分の都合の
良いように周辺の環境情報を操作する力がある…………。それが、わたしがここにいる理
由。あなたがここにいる理由…………………… 。

（待ってくれ。正直言おう、さっぱりわからない。）

信じて………………。

（そもそも、何で俺なんだ？いや、百歩譲ってお前の…その、情報なんとか体云々ってい
うのを信用したとして、なぜ俺に正体を明かすんだ？）

あなたは涼宮ハルヒに選ばれた・・・・・・・・・。涼宮ハルヒは意識的にしろ無意識的
にしろ、自分の意思を絶対的な情報として環境に影響を及ぼす………………。あなたが選
ばれたのには必ず理由がある……… 。

（ねぇよ。）

ある………………。あなたと涼宮ハルヒが、全ての可能性を握っている・・・・・・。

（マジで言ってるのか？）

もちろん………………。

『度を越えた無口な奴が、やっと喋るようになったかと思ったら、永延電波な事を言いや
がった。こんなトンデモ少女だったとは、さすがに想像外だぜ』

（あのな、そんな話なら直でハルヒに言った方が喜ばれると思うぞ。はっきり言うが、俺
はその手の話題には付いていけないんだ。悪いがな。）

情報統合思念体の意識の大部分は、涼宮ハルヒが自分の存在価値と能力を自覚してしまう
と予測できない危険を生む可能性があると認識している・・・。今はまだ様子を見るべき・・・・・。

（俺が今聞いたこと、ハルヒに伝えるかもしれないじゃないか）

彼女はあなたがもたらした情報を重視したりしない………………。

『確かに。』

情報統合思念体が地球に置いているインターフェイスは私一つではない………。情報統合
思念体の意識の一部は積極的の動きを起こして情報の変動を観測しようとしている… 。あ
なたは涼宮ハルヒにとっての鍵。危機が迫るとしたらまず、あなた………………。
"""
}

