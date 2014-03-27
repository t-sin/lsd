//// lsdファイルからWAVE音源をぶっこぬくコード
//lsd [file.lsd]



#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LSD "LSD"
#define MAGIC 4
#define LSD_HSIZE 4
#define LSD_HNAME 32
#define LSD_HEAD (LSD_HNAME + LSD_HSIZE)
#define LSD_WHEAD 18
#define LSD_NAZO 2

#define FILEMAX 48
#define DEFAULT_NAME "sound.lsd"

#define SURFIX ".wav"


//EXT_ALL=extract all files, EXT_ONE = extract specified file,
//OUT_LIST=output list, OUT_DET=output detail list
//enum OPTION {
//	EXT_ALL, EXT_ONE, OUT_LIST, OUT_DET
//};

///WAVE file header
struct WAVEHeader {
	//RIFF header
	char RIFF[4];  //RIFF magic number: "RIFF"
	int size;  //RIFF data size: fmt + data

	//WAVE header
	char WAVE[4];  //WAVE magic number: "WAVE"

	//fmt chunk
	char FMT[4];  //FMT chunk magic number: "fmt "
	int fmtSize;  //FMT chunk size [liner PCM: 16(0x100000)]
	short formatID;  //format ID [liner PCM: 1(0x0100)]
	short channelNum;  //channel number [monaural: 1(0x0100), stereo: 2(0x0200)]
	int samplingRate;  //sampling rate: (ex. 44.1kHz -> 44100(0x44ac0000))
	int dataSpeed;  //data speed[byte/sec] 44.1kHz, 16bit, stero -> 44100x2x2=176400(0x10b10200)
	short blockSize;  //block size[byte/sample*channels]: 16bit, stereo=2x2=4(0x0400)
	short samplingBit;  //sampling bit: 8bit(0x0800) or 16bit(0x1000)

	//data chunk
	char DATA[4];  //DATA chunk magic number: "data"
	int dataSize;  //wave data size
};

/*/file info
struct LsdInfo {
	char name[32];  //file name
	int size;  //data size
};*/

//file position in LSD
struct FilePos {
	char name[32];  //filename
	int h_start;  //WAVE header start position
	int h_end;  //WAVE header end position
	int d_start;  //data start position
	int d_end;  //data start posion
};

//archive infomation 
struct Files {
	int num;  //file number
	struct FilePos* pos;  //file info
};



/*/lsdのファイルをリストアップ
//finは開かれていること
//return: ファイル数
struct FileInfo* listFiles(int num) {
	struct FileInfo* files;
	return files;
}*/

//print FilesPos
void printFpos(struct FilePos* f) {
	fprintf(stderr, " file: \'%s\'\n", f->name);
	fprintf(stderr, " head[%x, %x], data[%x, %x]\n", f->h_start, f->h_end, f->d_start, f->d_end);
}

//print usage lsd.c
void usage() {
	fprintf(stderr,
		"usuage: lsd [-ln] [lsdfile]\n"
		//"\t-l: output filename list.\n"
		//"\t-L: output detail files.\n"
		//"\t-n [filename]: exctract specifid file\n"
	);
}


////global variables
//enum OPTION option;  //cmd option


//main()
int main(int argc, char* argv[]) {
	int i, ptr;
	
	char infile[256];  //input file name
	char buf[256];  //buffer
	int size;  //infile size
	struct Files files;  //files' infomation
	struct WAVEHeader wh;  //WAVE header
	FILE *fin, *fout;  //FILE

	
	//init 
	memset((void *)buf, 0, 256);
	strncpy(infile, DEFAULT_NAME, 256);
	files.num = 0;
	files.pos = calloc(FILEMAX, sizeof(struct FilePos));

	////arg check
	fprintf(stderr, "argc = %d\n", argc);
	if (argc < 2) {
		usage();
	} else {
		strncpy(infile, argv[1], 256);
	} /*else if (argc == 2) {
		if (argv[1][1] == '-') {
			switch(argv[1][2]) {
			  case 'l':
				option = OUT_LIST; break;
			  case 'L':
				option = OUT_DET; break;
			  case 'n':
				option = EXT_ONE; break;
			  default:
				;
			}
		} else {
			strncpy(infile, argv[1], 256);
		}
	} else {
		for (i=0; i<argc-1; i++) {
			if (argv[2][1] == '-') {
				switch(argv[2][2]) {
				  case 'l':
					option = OUT_LIST; break;
				  case 'L':
					option = OUT_DET; break;
				  case 'n':
					option = EXT_ONE; break;
				  default:
					;
				}
			} else {
				strncpy(infile, argv[2], 256);
			}
		}
	}*/

	//open infile
	fin = fopen(infile, "rb");
	if (fin == NULL) {
		fprintf(stderr, "cannot open file: %s\n", infile);
		exit(1);
	}
	fprintf(stderr, "open \'%s\'\n", infile);

	////infocheck
	//is infile LSD?
	if (fread(buf, 1, MAGIC, fin) < MAGIC || strcmp(buf, "LSD")) {
		fprintf(stderr, "\'%s\' is not LSD file\n", infile);
		exit(1);
	}
	fprintf(stderr, "magic = %s\n", buf);

	//file size check
	fseek(fin, 0, SEEK_END);
	size = ftell(fin);
	fprintf(stderr, "infile size = %d\n", size);

	//fileinfo check
	fseek(fin, MAGIC, SEEK_SET);
	for (i=0; i<FILEMAX; i++, files.num++) {
		int dsize;
		
		if (ftell(fin) >= size) {
			break;
		}
		//printf("LSDHeadpos = %d\n", ftell(fin));

		//get name
		fread((void *)buf, 1, LSD_HNAME, fin);
		strncpy(files.pos[i].name, buf, LSD_HNAME);
		
		//get size
		fread((void *)buf, 1, LSD_HSIZE, fin);
		dsize = (unsigned int)*(unsigned int *)buf;

		//get ptr
				//get header start
		files.pos[i].h_start = ftell(fin);
		files.pos[i].h_end = files.pos[i].h_start + LSD_WHEAD - LSD_NAZO - 1;
		files.pos[i].d_start = files.pos[i].h_start + LSD_WHEAD;
		files.pos[i].d_end = files.pos[i].h_start + LSD_WHEAD + dsize - 1;

		//seek next
		fseek(fin, LSD_WHEAD + dsize, SEEK_CUR);

		//fprintf(stderr, "i: %d\n", i);
		printFpos(&files.pos[i]);
	}
	fprintf(stderr, "file num = %d\n", files.num);
	
	
	////extract
	//prepair RIFF Header
	strncpy(wh.RIFF, "RIFF", MAGIC);
	strncpy(wh.WAVE, "WAVE", MAGIC);
	strncpy(wh.FMT, "fmt ", MAGIC);
	strncpy(wh.DATA, "data", MAGIC);

	//extract files
	for (i=0; i<files.num; i++) {
		//update wave header
		//fread((void *)buf, 1, LSD_WHEAD - LSD_NAZO);
		wh.fmtSize = 16;
		wh.dataSize = files.pos[i].d_end - files.pos[i].d_start -1;
		wh.size = 12 + 16 + 8 + wh.dataSize;
		

		//fout open
		strncpy(buf, files.pos[i].name, 256);
		strcat(buf, SURFIX);
		fout = fopen(buf, "wb");
		if (fout == NULL) {
			fprintf(stderr, "cannot open file \'%s\'\n", buf);
			exit(1);
		}

		//write header
		fprintf(stderr, "writing \'%s\'...\n", files.pos[i].name);
		fwrite(wh.RIFF, 1, 4, fout);
		fwrite(&wh.size, 1, 4, fout);
		fwrite(wh.WAVE, 1, 4, fout);
		fwrite(wh.FMT, 1, 4, fout);
		fwrite(&wh.fmtSize, 1, 4, fout);

		fseek(fin, files.pos[i].h_start, SEEK_SET);
		fread((void *)buf, 1, 16, fin);
		fwrite(buf, 1, 16, fout);
		
		fwrite(wh.DATA, 1, 4, fout);
		fwrite(&wh.dataSize, 1, 4, fout);

		//write data
		fseek(fin, files.pos[i].d_start, SEEK_SET);
		ptr = 0;
		while (wh.dataSize - ptr >= 256) {
			fread((void *)buf, 1, 256, fin);
			ptr += fwrite(buf, 1, 256, fout);
		}
		fread((void *)buf, 1, wh.dataSize - ptr, fin);
		fwrite(buf, 1, wh.dataSize - ptr, fout);

		//close
		fclose(fout);
	}
	fprintf(stderr, "finished extracting!!\n");

	////term
	free(files.pos);
	fclose(fin);
	
	return 0;
}
