#! /usr/bash

dosearchonerom() {
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
								printf("%08X 0x%X\n", unpack("N", substr($_, 0, 4)), $X);
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

dosearchonerom "$1"
