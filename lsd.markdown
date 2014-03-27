# lsd file format

某ゲーム製作サークルの音楽データアーカイブフォーマット。

## 詳細

下にlsdファイルのフォーマットを示す。
ここで`a | b`は連結を表す。

    LSD ::= MAGIC | SOUND_DATA*
    MAGIC ::= string [4 byte]: "LSD\0"
    
    SOUND_DATA ::= SOUND_NAME | SOUND_SIZE | WAVE
    SOUND_NAME ::= string [32 byte]
    SOUND_SIZE ::= unsigned integer [32 byte]
    
    WAVE ::= WAVE_HEADER | WAVE_DATA
    WAVE_HEADER ::= binary [16 byte]
    WAVE_DATA ::= binary [SOUND_SIZE + 2 byte]

始めの4byte`MAGIC`はマジックナンバーで、"LSD\0"で固定である。
それ以降にファイル情報(ファイル名)とWAVEファイル情報(波形データ)が連なっている。


## 補足

### SOUND_SIZEについて

`SOUND_SIZE`は`WAVE_DATA`の長さを示すと思われるが、なぜか2byte少ない。
いろいろ考えてみたものの、一向に理由が見えてこない。
こうしないと計算が合わないので、ぼくは考えるのをやめた。
(`SOUND_SIZE`には本来の`WAVE_DATA`のサイズから2を引いて格納する仕様なのだ、とする)

### WAVE_HEADERについて

`WAVE_HEADER`はWAVEファイルヘッダ情報の一部が格納されている。
WAVEファイルヘッダには以下の情報が含まれている。

1. フォーマットID
2. チャンネル数
3. サンプリングレート
4. データ速度 (Byte/sec)
5. ブロックサイズ
6. サンプルあたりのビット数 (bit/sample)

上記の他は固定である故に格納されていないのだろう。
逆を言えば、これらは必要な情報だ、ということである。

実際に、BGMと効果音ではサンプリング周波数と量子化ビット数が異なる。  
(BGMはステレオ44.1kHzの16bit、効果音はモノラル22.05kHzの8bitだったと思う)

