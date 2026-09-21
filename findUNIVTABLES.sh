#! /usr/bash

dosearchonerom0() {
	local therom="$1"
	perl -0777 -ne '
		$filelen = length($_);
		while (/(?{$X=pos()})(....)/sg) {
			$ProductInfoPtr = unpack("N", $1);
			if (($ProductInfoPtr & 3) == 0 && $ProductInfoPtr > 0 && $ProductInfoPtr < 8192) {
				$ProductInfoPos = $ProductInfoPtr + $X;
				if ($ProductInfoPos > 0 && $ProductInfoPos < $filelen) {
					($DecoderInfoPtr, $RamInfoPtr, $VideoInfoPtr, $NuBusInfoPtr, $HwCfgWord, $ProductKind, $DecoderKind, $ROM85World, $DefaultRSRCs, $ProductInfoVers)
					= unpack("NNNNnCCnCC", substr($_, $ProductInfoPos +  0, 24));
					if (
						($DecoderInfoPtr >= 0 && $DecoderInfoPtr < 8192) && 
						($RamInfoPtr > 4 && $RamInfoPtr < 8192) && 
						($VideoInfoPtr > 4 && $VideoInfoPtr < 8192) && 
						($NuBusInfoPtr > 4 && $NuBusInfoPtr < 8192) &&
						(($ProductKind >= 0 && $ProductKind <= 127) || $ProductKind >= 253) &&
						($DecoderKind >= 0 && $DecoderKind <= 30) &&
						($ROM85World == 0x3FFF || $ROM85World == 0x7FFF || $ROM85World == 0xFFFF) &&
						($DefaultRSRCs >= 1 && $DefaultRSRCs <= 4) &&
						($ProductInfoVers == 0 || $ProductInfoVers == 1 || $ProductInfoVers == 2) &&
						1
					) {
						$DecoderInfoPos = $DecoderInfoPtr + $ProductInfoPos;
						if ($DecoderInfoPos > 7 && $DecoderInfoPos < $filelen) {
							#$DecoderInfoVers = unpack("C", substr($_, $DecoderInfoPos - 7, 1));
							#if ($DecoderInfoVers == 1) {
								printf("checksum=%08X\n", unpack("N", substr($_, 0, 4)));
								printf("UnivTables=0x%X\n", $X);
								exit 0
							#}
						}
					}
				}
			}
		}
		exit 1
	' \
	"$therom"
}

#! /usr/bash

dosearchonerom1() {
	local therom="$1"
	LANG=C perl -0777 -ne '
		$filelen = length($_);
		$cpucount = 0;
		$productcount = 0;
		$decodercount= 0;
		printf("checksum=%08X\n", unpack("N", substr($_, 0, 4)));
		pos() = 0;
		while (/(?{$X=pos()})\x45\xf9(....)\x45\xfb\xa8\xf8\x58\x4a\x22\x12\x67\x00..\x43\xf2\x18\x00\xb0\x69/msg) {
			if (($X & 1) == 0) {
				printf("CPUIDProductLookup=0x%X\n", $X + unpack("N", $1) + 4);
				$cpucount += 1;
			}
		}
		pos() = 0;
		while (/(?{$X=pos()})\x41\xf9(....)\x41\xfb\x88\xf8\x58\x48\x20\x10\x67/msg) {
			if (($X & 1) == 0) {
				printf("ProductLookup=0x%X\n", $X + unpack("N", $1) + 4);
				$productcount += 1;
			}
		}
		pos() = 0;
		while (/(?{$X=pos()})\x43\xf9(....)\x43\xfb\x98\xf8\x20\x49\xd1\xd9\x24\x68/msg) {
			if (($X & 1) == 0) {
				printf("DecoderLookup=0x%X\n", $X + unpack("N", $1));
				$decodercount += 1;
			}
		}
		if ($cpucount > 1 || $productcount > 1 || $decodercount > 1) {
			printf("result=\"(duplicates)\"\n");
		}
		exit 0
	' \
	"$therom"
}

method=1
if [[ $1 = -old ]]; then
	method=0
	shift
fi

if ((method==1)); then
	dosearchonerom1 "$1"
else
	dosearchonerom0 "$1"
fi
