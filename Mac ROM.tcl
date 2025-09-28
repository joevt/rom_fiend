#### Macintosh DeclROM and System ROM format for Hex Fiend
# Copyright (c) 2022-2023 Eric Harmon

# Note: data types are described in the card manual, page 152

#### Hex Fiend setup

hf_min_version_required 2.15
big_endian

#### ROM location setup

# ROMs larger than 3MiB have their DeclROMs at the 3MiB boundary
# TODO: For some DeclROMs and System ROMs there's extra data at the end, we need to read the
# variety of possible locations instead of just the end.
if {[len] > 3145728} {
	set end_of_rom 3145728
} else {
	set end_of_rom [len]
}

#### Detect scrambled ROMs and find end of DeclROM

# TODO: We're not detecting all conditions, just the ones I've found
goto 2
set magic [uint32]
if {$magic == 0x38D46CA5} {
	entry "ERROR" "ROM must be byte-wise reversed and XOR'd with 0xFF"
	return
}

set offset [len]
while {$offset > 0} {
	goto [expr $offset - 6]
	set magic [uint32]
	if {$magic == 0x5A932BC7} {
		set end_of_rom [expr $offset]
		break
	} elseif {$magic == 0x935AC72B} {
		entry "ERROR" "ROM must be byte-swapped from little-endian"
		return
	} elseif {$magic == 0xA56CD438} {
		entry "ERROR" "ROM must be XOR'd with 0xFF"
		return
	} elseif {$magic == 0x7878} {
		entry "ERROR" "ROM is repeated across bytelanes, take every 4th byte"
		return
	}
	set offset [expr $offset - 0x10000]
}

# Reset state
goto 0
# TODO: It's unfortunate this is a global
set rom_date -1

#### File matching

# Currently we don't require this, since we can parse System ROMs as well -- if it's not a DeclROM or a System ROM it'll be caught way below as unsupported
#requires [expr [len]-6] "5a 93 2b c7"

#### Functions

## Utility functions

set symbolsdict [dict create]

proc field_name_and_symbol {prefix addr} {
	global symbolsdict
	if {[dict exists $symbolsdict $addr]} {
		return [format "%s (%s)" $prefix [dict get $symbolsdict $addr]]
	} else {
		return $prefix
	}
}

proc offsetname {addr} {
	return [field_name_and_symbol [format "-> 0x%X" $addr] $addr]
}

proc pointerentry {name addr size} {
	entry $name [offsetname $addr] $size
	move $size
}

proc codeentry {name addr size} {
	if {$addr < 0 || $addr >= [len]} {
		pointerentry $name $addr $size
	} else {
		section -collapsed $name
			sectionvalue [offsetname $addr]
			pointerentry $name $addr $size
			set returnpos [pos]
			goto $addr
			if {$addr > 0 && $addr < [len]} {
				bytes 2 "$name start"
			}
		endsection
		goto $returnpos
	}
}

proc dooffset {name origin size offset} {
	set addr $offset
	if {$offset != 0} {
		set addr [expr $origin + $offset]
		move -$size
		pointerentry $name $addr $size
	}
	return $addr
}

proc offset32 {name origin} {
	return [dooffset $name $origin 4 [int32]]
}

proc offset24 {name origin} {
	return [dooffset $name $origin 3 [int24]]
}

proc offset16zero {name origin} {
	set offset [int16]
	set addr [expr $origin + $offset]
	move -2
	pointerentry $name $addr 2
	return $addr
}

proc dooffsetcode {name origin size offset} {
	set addr $offset
	if {$offset != 0} {
		set addr [expr $origin + $offset]
		move -$size
		codeentry $name $addr $size
	}
	return $addr
}

proc offset32code {name origin} {
	if {[pos] <= [len] - 4} {
		return [dooffsetcode $name $origin 4 [int32]]
	}
	return 0
}

proc dooffsetsection {name origin size offset} {
	set addr $offset
	if {$offset != 0} {
		set addr [expr $origin + $offset]
		if {$addr > 0 && $addr < [len]} {
			move -$size
			section -collapsed $name
			sectionvalue [offsetname $addr]
			pointerentry "$name offset" $addr $size
			return $addr
		}
	}
	return 0
}

proc offset32section {name origin} {
	set offset [uint32]
	if {($origin == 0) && (
		(($offset & 0xFFF00000) == 0x40800000) || (($offset & 0xFFF00000) == 0x400000)
	)} {
		return [dooffsetsection $name 0 4 [expr $offset & 0xFFFFF]]
	} else {
		move -4
		return [dooffsetsection $name $origin 4 [int32]]
	}
}

proc offset24section {name origin} {
	return [dooffsetsection $name $origin 3 [int24]]
}

# Add support for pstr
proc pstr {args} {
	set len [uint8]
	set value ""
	if {$len > 0 && [llength $args] > 0} {
		set value [str $len [lindex $args 0]]
	}
	if {[llength $args] > 1} {
		move [expr -1 - $len]
		entry [lindex $args 1] $value [expr $len + 1]
		move [expr $len + 1]
	}
	return $value;
}

# Add support for int24s
# TODO: A better way? Ultimately we want an int24 and HexFiend only supports uint24
# TODO: Support -hex
# So we read the first part to get the signing information, then read the second part and bit shift everything into place
proc int24 {args} {
	set first [int16]
	set second [uint8]
	set value [expr $first << 8 | $second]
	if {[llength $args] > 0} {
		move -3
		entry [lindex $args 0] $value 3
		move 3
	}
	return $value
}

# Add support for 32-bit fixed point fractionals
proc fixed32 {args} {
	set whole [uint16]
	set fraction [uint16]
	set value [expr $whole + $fraction*0.1]
	if {[llength $args] > 0} {
		move -4
		entry [lindex $args 0] $value 4
		move 4
	}
	return $value
}

# Read a jump vector, which may be a JMP or a BRA
proc jmp {args} {
	set instruction [uint16]
	set addr 0
	set size 4
	if {$instruction == 0x4EFA} {
		#JMP.W
		set offset [int16]
		set addr [expr $offset + [pos] - 2]
	} elseif {$instruction == 0x6000} {
		#BRA.W
		set offset [int16]
		set addr [expr $offset + [pos] - 2]
	} elseif {$instruction == 0x60FF || $instruction == 0x61FF} {
		#BRA.L, BRS.L
		set offset [int32]
		set addr [expr $offset + [pos] - 4]
		set size 6
	} else {
		move -2
		set addr [int32]
	}
	if {[llength $args] > 0} {
		move -$size
		codeentry [lindex $args 0] $addr $size
	}
	return $addr
}

proc sort_dict_by_int_value {dict args} {
	set lst {}
	dict for {k v} $dict {lappend lst [list $k $v]}
	return [concat {*}[lsort -integer -index 1 {*}$args $lst]]
}

# Detemrine if a ROM uses the universal format
proc universal_rom {version} {
	if {$version < 0x6} {
		return false
	}
	return true
}

# Determine if a ROM uses the legacy resources format
proc legacy_resources {version} {
	if {$version >= 0x75 && $version < 0x7C} {
		return true
	}
	return false
}

## Human readable text parsers

# Given raw byte lane data, translate to human readable
proc byte_lanes {input_lanes} {
	# TODO: Could we read bits instead of just hardcoding the documented table?
	switch $input_lanes {
		225 {
			# 0xE1
			set lanes "0"
		}
		210 {
			# 0xD2
			set lanes "1"
		}
		195 {
			# 0xC3
			set lanes "0,1"
		}
		180 {
			# 0xB4
			set lanes "2"
		}
		165 {
			# 0xA5
			set lanes "0,2"
		}
		150 {
			# 0x96
			set lanes "1,2"
		}
		135 {
			# 0x87
			set lanes "0,1,2"
		}
		120 {
			# 0x78
			set lanes "3"
		}
		105 {
			# 0x69
			set lanes "0,3"
		}
		90 {
			# 0x5A
			set lanes "1,3"
		}
		75 {
			# 0x4B
			set lanes "0,1,3"
		}
		60 {
			# 0x3C
			set lanes "2,3"
		}
		45 {
			# 0x2D
			set lanes "0,2,3"
		}
		30 {
			# 0x1E
			set lanes "1,2,3"
		}
		15 {
			# 0x0F
			set lanes "0,1,2,3"
		}
		default {
			set lanes "Unknown"
		}
	}
	return $lanes
}

# Given a resource type, return the human readable type
proc rsrc_type {input_type} {
	# Where possible, from ROMDefs.inc
	switch $input_type {
		1 {
			set type "Board"
		}
		2 {
			set type "Test"
		}
		3 {
			set type "Display"
		}
		4 {
			set type "Network"
		}
		6 {
			# TODO: Confirm
			set type "Communications"
		}
		8 {
			# Confusingly this is used for capture cards, etc
			# Per the docs, "scanners bring in data somehow"
			# TODO: Possible cTypes are 1 for....video?
			set type "Scanner"
		}
		9 {
			# TODO: Confirm, the Kanji board and WGS card use these, Kanji for ROM, WGS for Memory
			# TODO: Linux calls this "font" but I think they're wrong based on how WGS uses this
			set type "Memory"
		}
		10 {
			set type "CPU"
		}
		12 {
			# SCSI cards use this, but officially it's the "intelligent bus"
			set type "IntBus"
		}
		17 {
			set type "Proto"
		}
		19 {
			# TODO: The PC Drive card claims to be an "intelligent bus" but uses 19, so...let's assume for now this is the same for some reason
			set type "IntBus"
		}
		25 {
			# TODO: Confirm, Interware Video CD Player uses this for MPEG decoder, PowerVideo AV for JPEG decoder
			set type "Compression Accelerator(?)"
		}
		32 {
			# PowerBook Duo System page 184 (204)
			set type "Dock"
		}
		default {
			# TODO: Categories include display, network, terminal emulator, serial, parallel, intelligent bus, and human input devices.
			# TODO: The followng types may be correct:
			#  - 7: Printer controller? -- LasterMAX uses it
			#  - 12: Disk
			#  - 17: Debug Card?
			#  - 10: Floppy Disk?
			#  		- With cType 2 for MFM?
			set type "Unknown"
		}
	}
	return $type
}

# Resolve cTypes to human readable
proc resolve_ctype {category ctype} {
	switch $category {
		1 {
			# Board always seems to be zero, so just call that...Board
			switch $ctype {
				0 {
					# Cards page 148
					set type "Board (0)"
				}
				default {
					set type "Unknown ($ctype)"
				}
			}
		}
		3 {
			# Validated by looking at Display cards
			switch $ctype {
				1 {
					set type "Video (1)"
				}
				2 {
					# Cards page 148
					set type "LCD (2)"
				}
				5 {
					# Aapps MicroTV
					set type "TV (5)"
				}
				default {
					set type "Unknown ($ctype)"
				}
			}
		}
		4 {
			# Validated by looking at Ethernet cards
			switch $ctype {
				1 {
					set type "EtherNet (1)"
				}
				2 {
					# TODO: Linux claims this is RS232, but TokenTalk seems correct since the TokenRing card uses it
					set type "TokenTalk (2)"
				}
				7 {
					set type "Token Ring 802.2? (4)"
				}
				default {
					# Possible others:
					# AppleTalk
					# DECNet
					set type "Unknown ($ctype)"
				}
			}
		}
		6 {
			# Validated by looking at Communications cards
			switch $ctype {
				2 {
					set type "RS232 (2)"
				}
				6 {
					# VersaLink
					set type "RS422 (6)"
				}
				8 {
					# Referenced by TattleTech and the Mac Mainframe II
					set type "IBM 3270 (8)"
				}
				10 {
					# Apple Coax/Twinax Card
					set type "IBM 5250 (10)"
				}
				11 {
					set type "Centronics (11)"
				}
				19 {
					# Apple Coax/Twinax Card
					set type "IBM 3270 (19)"
				}
				27 {
					# SCii RNIS
					set type "ISDN (27)"
				}
				30 {
					# KMW Systems TwinAxcess
					set type "IBM 5250/Twinax (30)"
				}
				default {
					# Possible others:
					# Parallel
					# MIDI
					set type "Unknown ($ctype)"
				}
			}
		}
		8 {
			# TODO: 1 is Video digitizer and 2 is Audio?
			# Scanner
			# Image Digitizer
			# Audio Signal Processor
			# Optical Scanner
			set type "Unknown ($ctype)"
		}
		10 {
			switch $ctype {
				1 {
					# Card docs page 173
					set type "68000 (1)"
				}
				2 {
					# Guess, by process of elimination above and below
					set type "68010 (2)"
				}
				3 {
					# Linux nubus.h -- also present in Apple headers but ambiguously
					set type "68020 (3)"
				}
				4 {
					# Linux nubus.h -- also present in Apple headers but ambiguously
					set type "68030 (4)"
				}
				5 {
					# Radius Rocket
					set type "68040 (5)"
				}
				11 {
					# MacIvory III: https://oldvcr.blogspot.com/2024/10/refurb-weekend-symbolics-macivory-lisp.html
					# TODO: It's possible this is all Lisp processors
					set type "Symbolics Ivory (11)"
				}
				20 {
					# Team ASA Raven
					set type "i860 (20)"
				}
				21 {
					set type "AppleII (21)"
				}
				36 {
					# Reply Houdini II
					set type "80486 (36)"
				}
				default {
					# Possible others:
					# 68010
					# 8086
					# 80286
					# 80386
					set type "Unknown ($ctype)"
				}
			}
		}
		12 {
			# Validated by looking at PLI card
			switch $ctype {
				8 {
					set type "SCSI (8)"
				}
				default {
					set type "Unknown ($ctype)"
				}
			}
		}
		19 {
			# PC Drive card
			# TODO: Confirm
			switch $ctype {
				2 {
					set type "MFM (2)"
				}
				default {
					set type "Unknown ($ctype)"
				}
			}
		}
		25 {
			# TODO: These are a guess based on Interware cards
			switch $ctype {
				257 {
					set type "JPEG (257)"
				}
				259 {
					set type "MPEG (259)"
				}
				default {
					set type "Unknown ($ctype)"
				}
			}
		}
		17 {
			switch $ctype {
				# ROMEqu.a
				256 {
					set type "Debugger (256)"
				}
				default {
					set type "Unknown ($ctype)"
				}
			}
		}
		32 {
			# PowerBookDuo System page 184 (204)
			switch $ctype {
				1 {
					set type "Dock Station (1)"
				}
				2 {
					set type "Dock Desk (2)"
				}
				3 {
					set type "Dock Travel (3)"
				}
			}
		}
		default {
			set type "Unknown ($ctype)"
		}
	}
	return $type
}

# Resolve drSW to human readable
proc resolve_drsw {drSW} {
	switch $drSW {
		1 {
			set type "Apple (1)"
		}
		260 {
			# ROMEqu.a
			set type "MacsBug (260)"
		}
		65535 {
			# Card docs page 173
			set type "Not in ROM (65535)"
		}
		default {
			# TODO: These were assigned by Apple so we can encode known ones
			set type "$drSW"
		}
	}
}

# Given a CPU type, return the human readable type
proc cpu_type {input_type} {
	# NuBus reference page 167 (208)
	# ROMDefs.h for PPC
	switch $input_type {
		1 {
			set type "68000 (1)"
		}
		2 {
			set type "68020 (2)"
		}
		3 {
			set type "68030 (3)"
		}
		4 {
			set type "68040 (4)"
		}
		37 {
			# TODO: This might be for CPU sResources and not drivers?
			set type "PowerPC 601 (37)"
		}
		46 {
			# TODO: This might be for CPU sResources and not drivers?
			set type "PowerPC 603 (46)"
		}
		default {
			set type "Unknown ($input_type)"
		}
	}
	return $type
}

# Convert timing map values to human readable
proc timing_map {timing} {
	# TODO: Add all the timings from Video.h
	# TODO: Also see DeclData.r...what are those sName for?
	switch $timing {
		0 {
			# TODO: zero entry is a bit unclear: "Unknown timing... force user to confirm."
			set type "User Specified(?) (0)"
		}
		8 {
			set type "Thunder/24 (Buggy) (8)"
		}
		42 {
			set type "Fixed Rate LCD (42)"
		}
		130 {
			set type "512x384 (60 Hz) (130)"
		}
		140 {
			set type "640x480 (67 Hz) (140)"
		}
		160 {
			set type "640x870 (75 Hz) (160)"
		}
		170 {
			set type "832x624 (75 Hz) (170)"
		}
		210 {
			set type "1024x768 (75 Hz) (210)"
		}
		220 {
			set type "1152x870 (75 Hz) (220)"
		}
		230 {
			set type "\[NTSC\] 512x384 (60 Hz, interlaced, non-convolved) (230)"
		}
		232 {
			set type "\[NTSC\] 640x480 (60 Hz, interlaced, non-convolved) (232)"
		}
		238 {
			set type "\[PAL\] 640x480 (50 Hz, interlaced, non-convolved) (238)"
		}
		240 {
			set type "\[PAL\] 768x576 (50 Hz, interlaced, non-convolved) (240)"
		}
		280 {
			set type "1600x1200 (60 Hz) (280)"
		}
		510 {
			set type "1920x1080 (60 Hz) (510)"
		}
		default {
			set type "Unknown ($timing)"
		}
	}
	return $type
}

# Parse block transfer bit tables into human readable
proc parse_transfer_bits {} {
	uint16_bits 0 "2-bit Transfer Supported"
	move -2
	uint16_bits 1 "4-bit Transfer Supported"
	move -2
	uint16_bits 2 "8-bit Transfer Supported"
	move -2
	uint16_bits 3 "16-bit Transfer Supported"
}

# Read resource combo mask data into human readable
proc combos {combo} {
	# TODO: From tbxi, but these don't seem quite right. These seem like mask bits and not fixed values, but they're inconsistent.
	# TODO: It's a bit odd these are so long when only the first few bits are used...
	# TODO: This only works with masks that are 8 bytes
	switch $combo {
		0x0100000000000000 {
			set name "NetBoot (0x10)"
		}
		0x0800000000000000 {
			set name "Apple Talk 2.0 + NetBoot (0x08)"
		}
		0x2000000000000000 {
			set name "Apple Talk 2.0 (0x20)"
		}
		0x3000000000000000 {
			set name "Apple Talk 2.0 + NetBoot + FPU (0x30)"
		}
		0x4000000000000000 {
			set name "Apple Talk 1.0 (0x40)"
		}
		0x7800000000000000 {
			set name "Universal Resource (0x78)"
		}
		default {
			set name "Unknown ($combo)"
		}
	}
	return $name
}

# Parse ROM version into human readable
proc rom_version {version} {
	set major [expr $version >> 4]
	set minor [expr $version & 0x0F]
	set hex_version [format %X $version]

	return "$major.$minor ($$hex_version/$version)"
}

# Parse ROM release version into human readable
proc rom_release {version} {
	set major [expr $version >> 12]
	set minor [expr ($version & 0x0F00) >> 8]
	set letter [format %x [expr ($version & 0x00F0) >> 4]]
	set build [expr $version & 0x000F]
	set hex_version [format 0x%X $version]

	return "$major.$minor$letter$build ($hex_version)"
}

## sResource type parsers

# Compute the vendor info
proc vendor_info {offset} {
	set temp_location [pos]
	move $offset
	section "Vendor Info"
	set vendor_rsrc_offset 0x01
	set vendor_rsrc_type 0x00
	while {[expr $vendor_rsrc_offset != 0x000000 && $vendor_rsrc_type != 0xFF]} {
		section -collapsed "Metadata"
		set vendor_rsrc_type [uint8 "Type"]
		set vendor_rsrc_offset [int24 "Offset"]
		endsection
		set vendor_rsrc_entry_return [pos]
		move [expr $vendor_rsrc_offset-4]
		# NuBus documentation, page 178 (219)
		switch $vendor_rsrc_type {
			1 {
				set vendorid [cstr "macroman" "VendorID"]
			}
			2 {
				cstr "macroman" "SerialNum"
			}
			3 {
				cstr "macroman" "RevLevel"
			}
			4 {
				cstr "macroman" "PartNum"
			}
			5 {
				# Stash the date location for later
				global rom_date
				set rom_date [pos]

				cstr "macroman" "Date"
			}
		}
		goto $vendor_rsrc_entry_return
	}
	goto $temp_location
	endsection
	return $vendorid
}

# Examine a driver directory for directoriesdata
proc driver_dir {offset} {
	set temp_location [pos]
	move $offset
	section "Drivers"
	set driver_rsrc_offset 0x01
	set driver_rsrc_type 0x00
	while {[expr $driver_rsrc_offset != 0x000000 && $driver_rsrc_type != 0xFF]} {
		section "Driver"
		set driver_rsrc_type [uint8]
		set decoded_cpu [cpu_type $driver_rsrc_type]
		sectionname "Driver ($driver_rsrc_type)"
		move -1
		entry "CPU ID" $decoded_cpu 1
		move 1
		set driver_rsrc_offset [int24 "Offset"]
		if {$driver_rsrc_type == 0xFF} {
			sectionname "Terminator (255)"
			sectioncollapse
		} else {
			# TODO: Not sure this extraction is correct, or at least it's insufficient
			# "For the Macintosh Operating System, this structure is described in detail with the Device Manager information in Inside Macintosh."
			# Inside Macintosh, Device Manager page 1-13
			set driver_rsrc_entry_return [pos]
			move -4
			move $driver_rsrc_offset
			move 1
			set driver_length [uint24 "Physical Block Size"]
			# TODO: Assuming we subtract 4 because the length includes the header where the length is specified
			#bytes [expr $driver_length-4] "Driver Data"
			section "Driver Data"
			section -collapsed "Header"
			set driver_region_start [pos]
			uint16 "drvrFlags"
			uint16 "drvrDelay"
			uint16 "drvrEMask"
			uint16 "drvrMenu"
			set open_offset [uint16 "drvrOpen"]
			set prime_offset [uint16 "drvrPrime"]
			set control_offset [uint16 "drvrCtl"]
			set status_offset [uint16 "drvrStatus"]
			set close_offset [uint16 "drvrClose"]
			set name [pstr "macroman" "drvrName"]
			endsection

			section "Functions"
			# The driver functions can appear in any order, or be omitted entirely (offset == 0), so to determine the size of each function block (approximately)
			# we have to sort all the offsets to determine the order of the function calls,
			set offset_dict [dict create "Open" $open_offset "Prime" $prime_offset "Control" $control_offset "Status" $status_offset "Close" $close_offset]
			set sorted_offsets [sort_dict_by_int_value $offset_dict]
			set current_offset 0
			set current_type ""
			dict for {type value} $sorted_offsets {
				if {$value > 0} {
					if {$current_offset > 0} {
						goto $driver_region_start
						move $current_offset
						# TODO: We can read right off the edge so catch errors
						set status [catch {
							bytes [expr $value-$current_offset] $current_type
						} err]
						if {$status} {
							entry "ERROR" $err
						}
						# Debugging:
						#entry $current_type "$value - $current_offset"
					}
					set current_type $type
					set current_offset $value
				}
			}
			# For the last entry, it must span from the $current_offset to the $driver_length
			if {$current_offset > 0} {
					goto $driver_region_start
					move $current_offset
					# TODO: Is this _really_ correct? It seems to work but this is ugly -- we're just compensating for offset mistakes earlier
					# TODO: We seem to get Micron driver wrong still, so there's more work to do -- it's possible this is NEVER exact, because the OS doesn't care as long as the entry points work
					# TODO: Sometimes we read right off the end of the file (Spectrum 8 Series III), so catch errors
					set status [catch {
						bytes [expr $driver_length-4-$current_offset+12] $current_type
					} err]
					if {$status} {
						entry "ERROR" $err
					}
					# Debugging:
					#entry $current_type "$value - $current_offset"
			}
			endsection
			endsection
			sectionname "Driver ($driver_rsrc_type) ($name)"
			goto $driver_rsrc_entry_return
		}
		endsection
	}
	goto $temp_location
	endsection
}

# Examine the gamma data
proc gamma_dir {offset} {
	set temp_location [pos]
	move $offset
	set gamma_rsrc_offset 0x01
	set gamma_rsrc_type 0x00
	while {[expr $gamma_rsrc_offset != 0x000000 && $gamma_rsrc_type != 0xFF]} {
		section -collapsed "Gamma Entry"
		set gamma_rsrc_type [uint8 "Type"]
		set gamma_rsrc_offset [int24 "Offset"]
		if {$gamma_rsrc_type == 0xFF} {
			sectionname "Terminator (255)"
		} else {
			# TODO: Something is weird, sometimes we get bad entries with nonsense offsets
			set gamma_rsrc_entry_return [pos]
			move [expr $gamma_rsrc_offset-4]
			set start [pos]
			set length [uint32 "Record Length"]
			uint16 "ID"
			set name [cstr "macroman" "Name"]
			sectionname $name
			set end [pos]
			bytes [expr $length - ($end - $start)] "Gamma Image"
			goto $gamma_rsrc_entry_return
		}
		endsection
	}
	goto $temp_location
}

# Examine the sVidParm data
proc svidparam_dir {offset} {
	set temp_location [pos]
	move $offset
	set svidparam_rsrc_offset 0x01
	set svidparam_rsrc_type 0x00
	while {[expr $svidparam_rsrc_offset != 0x000000 && $svidparam_rsrc_type != 0xFF]} {
		section -collapsed "Vid Param"
		set svidparam_rsrc_type [uint8 "Type"]
		sectionname "Video Mode $svidparam_rsrc_type"
		set svidparam_rsrc_offset [int24 "Offset"]
		if {$svidparam_rsrc_type == 0xFF} {
			sectionname "Terminator (255)"
		} else {
			set meh [pos]
			# TODO: Isn't this off by 4?
			move $svidparam_rsrc_offset
			uint32 "TODO"
			goto $meh
		}
		endsection
	}
	goto $temp_location
}

proc smemory {offset} {
	set temp_location [pos]
	move $offset
	set smemory_rsrc_offset 0x01
	set smemory_rsrc_type 0x00
	while {[expr $smemory_rsrc_offset != 0x000000 && $smemory_rsrc_type != 0xFF]} {
		section -collapsed "sMemory Entry"
		set smemory_rsrc_type [uint8 "Type"]
		set smemory_rsrc_offset [int24 "Offset"]
		switch $smemory_rsrc_type {
			1 {
				# TODO: This is redundant with the other sRsrcType parse, should be merged
				# TODO: Is category "150" just "Memory"?
				sectionname "sRsrcType (1)"
				set temp [pos]
				move -4
				move $smemory_rsrc_offset
				set category [uint16]
				move -2
				entry "Category" [rsrc_type $category] 2
				move 2
				# TODO: "The cType field is a subclass within a category. Within display devices, for example, are video cards and graphics extension processors; within networks, AppleTalk and Ethernet."
				# TODO: So we should categorize the resources correctly based on the top-level Category.
				set ctype [uint16]
				move -2
				entry "cType" [resolve_ctype $category $ctype] 2
				move 2
				set drSW [uint16]
				move -2
				entry "DrSW" [resolve_drsw $drSW] 2
				move 2
				# This *should* be non-unique across smemorys, so we can't do anything but list the number
				uint16 "DrHW"
				goto $temp
			}
			2 {
				set smemory_rsrc_entry_return [pos]
				move [expr $smemory_rsrc_offset-4]
				set name [cstr "macroman" "Name"]
				sectionname "sRsrcName (2)"
				goto $smemory_rsrc_entry_return
			}
			128 {
				sectionname "MinorRAMAddr (128)"
				entry "TODO" 0
				# TODO
			}
			129 {
				sectionname "MajorRAMAddr (129)"
				entry "TODO" 0
				# TODO
			}
			130 {
				sectionname "MinorROMAddr (130)"
				entry "TODO" 0
				# TODO
			}
			131 {
				sectionname "MajorROMAddr (131)"
				entry "TODO" 0
				# TODO
			}
			132 {
				sectionname "MinorDeviceAddr (132)"
				entry "TODO" 0
				# TODO
			}
			133 {
				sectionname "MajorDeviceAddr (133)"
				entry "TODO" 0
				# TODO
			}
			255 {
				sectionname "Terminator (255)"
			}
			default {
				sectionname "Unknown ($smemory_rsrc_type)"
			}
		}
		endsection
	}
	goto $temp_location
}

# Discover the video mode names
proc vid_names {offset} {
	set temp_location [pos]
	move $offset
	set vid_names_rsrc_offset 0x01
	set vid_names_rsrc_type 0x00
	while {[expr $vid_names_rsrc_offset != 0x000000 && $vid_names_rsrc_type != 0xFF]} {
		section -collapsed "Video Mode"
		set vid_names_rsrc_type [uint8 "Type"]
		set vid_names_rsrc_offset [int24 "Offset"]
		# TODO: Does this mean something else?
		if {$vid_names_rsrc_type == 0} {
			sectionname "Invalid (0)"
		} elseif {$vid_names_rsrc_type == 0xFF} {
			sectionname "Terminator (255)"
		} else {
			set vid_names_rsrc_entry_return [pos]
			move [expr $vid_names_rsrc_offset-4]
			uint32 "Record Length"
			# TODO: Confirm
			uint16 "Localization ID"
			set name [cstr "macroman" "Name"]
			sectionname $name
			goto $vid_names_rsrc_entry_return
		}
		endsection
	}
	goto $temp_location
}

# Discover the video modes
proc vid_mode {offset} {
	set temp_location [pos]
	move $offset
	set vid_mode_rsrc_offset 0x01
	set vid_mode_rsrc_type 0x00
	while {[expr $vid_mode_rsrc_offset != 0x000000 && $vid_mode_rsrc_type != 0xFF]} {
		section -collapsed "Metadata"
		set vid_mode_rsrc_type [uint8 "Type"]
		set vid_mode_rsrc_offset [int24 "Offset"]
		set vid_mode_rsrc_entry_return [pos]
		endsection
		move -4
		# NuBus documentation, page 178 (219)
		switch $vid_mode_rsrc_type {
			1 {
				# TODO: Properly mark out this offset
				move $vid_mode_rsrc_offset
				section "Mode Data"
				uint32 "Record Size"
				uint32 "vpBaseOffset"
				uint16 "vpRowBytes"
				set bounds0 [uint16 "vpBounds(0)"]
				set bounds1 [uint16 "vpBounds(1)"]
				set bounds2 [uint16 "vpBounds(2)"]
				set bounds3 [uint16 "vpBounds(3)"]
				uint16 "vpVersion"
				uint16 "vpPackType"
				# Table 9-2 is incorrect, this is a full byte
				uint32 "vpPackSize"
				fixed32 "vpHRes"
				fixed32 "vpVRes"
				uint16 "vpPixelType"
				uint16 "vpPixelSize"
				uint16 "vpCmpCount"
				uint16 "vpCmpSize"
				uint32 "vpPlaneBytes"
				endsection
			}
			2 {
				# TODO: mTable: Offset to the device color table for fixed CLUT devices; mTable has the same format as the cTabHandle structure, described with the Color Manager information in Inside Macintosh.
				uint32 "mTable Offset"
			}
			3 {
				move 1
				uint24 "Page Count"
			}
			4 {
				move 1
				uint24 "Device Type"
			}
			# TODO: Confirm
			5 {
				move 1
				# From ROMDefs.h: slot block xfer info PER MODE
				uint24 "mBlockTransferInfo"
			}
			# TODO: Confirm
			6 {
				move 1
				# From ROMDefs.h: slot max. locked xfer count PER MODE
				uint24 "mMaxLockedTransferCount"
			}
			default {
				# TODO: Mark terminator
			}
		}
		goto $vid_mode_rsrc_entry_return
	}
	goto $temp_location

	set height [expr $bounds2 - $bounds0]
	set width [expr $bounds3 - $bounds1]

	return "$width x $height"
}

# Examine a block describing an executable section
proc exec_block {offset} {
	# TODO: Need to verify this block is correct
	set temp_location [pos]
	move $offset
	set length [uint32 "Length"]
	set revision [uint8]
	move -1
	if {$revision == 2} {
		entry "Revision" "sExec2 (2)" 1
		move 1
	} else {
		uint8 "Revision"
	}
	set raw_cpu [uint8]
	move -1
	entry "CPU ID" [cpu_type $raw_cpu] 1
	move 1
	set reserved [uint16]
	if {$reserved != 0} {
		move -2
		entry "Reserved" [format "0x%X" $reserved] 2
		move 2
	}
	set second_offset [int32 "Offset"]
	move -4

	move $second_offset
	# TODO: This calculation is WEIRD, but it seems correct?
	bytes [expr $length-$second_offset-8] "Code"
	goto $temp_location
}

# Read the auxiliary video parameters
proc vid_aux_params {offset} {
	set temp_location [pos]
	move $offset
	set vid_aux_params_rsrc_offset 0x01
	set vid_aux_params_rsrc_type 0x00
	while {[expr $vid_aux_params_rsrc_offset != 0x000000 && $vid_aux_params_rsrc_type != 0xFF]} {
		section -collapsed "Mode"
		section -collapsed "Metadata"
		set vid_aux_params_rsrc_type [uint8 "Type"]
		set vid_aux_params_rsrc_offset [int24 "Offset"]
		endsection
		if {$vid_aux_params_rsrc_type == 0xFF} {
			sectionname "Terminator (255)"
		} else {
			sectionname "Mode $vid_aux_params_rsrc_type"
			set vid_aux_params_rsrc_entry_return [pos]
			move [expr $vid_aux_params_rsrc_offset-4]
			uint32 "Unknown"
			set timing_info [uint32]
			move -4
			entry "Timing" [timing_map $timing_info] 4
			goto $vid_aux_params_rsrc_entry_return
		}
		endsection
	}
	goto $temp_location
}

# Examine the block transfer info block
proc block_transfer_info {offset} {
	set temp_location [pos]
	move $offset
	section "Master"
	# Master word
	uint16_bits 15 "Is Master"
	move -2
	uint16_bits 14 "Locked Transfer Supported"
	move -2
	# TODO: This is 'Reserved' so technically we don't need to parse it, but we should be reading the bit values
	#uint16_bits 4,5,6 "Format"
	#move -2
	parse_transfer_bits
	endsection
	section "Slave"
	# Slave word
	uint16_bits 15 "Is Slave"
	move -2
	parse_transfer_bits
	endsection
	goto $temp_location
}

# Determine if an offset is logical -- if it doesn't offset far enough to leave the directory entry, it's not valid
proc valid_rsrc_dir_offset {offset} {
	if {$offset > 0 && $offset < 4} {
		return 0
	} else {
		return 1
	}
}

# Parse the sResourceDir
proc parse_rsrc_dir {directory} {
	# Jump into the directory and start parsing it's entries
	goto $directory
	set rsrc_offset 1
	set rsrc_type 0

	# Loop over the top level sResource entries
	while {[expr $rsrc_offset != 0x000000 && $rsrc_type != 0xFF]} {
		section -collapsed "sResource"

		# These will be filed in by the type record, which is usually first
		# TODO: If it's not before the video entries, we won't parse them correctly
		set category 0
		set ctype 0
		set drSW 0
		set rsrc_name ""

		section -collapsed "Metadata"
		set rsrc_type [uint8 "Type"]
		set rsrc_offset [int24 "Offset"]
		endsection
		set location [pos]
		if {$rsrc_type == 0xFF} {
			sectionname "Terminator (255)"
		} else {
			move [expr $rsrc_offset-4]

			set sub_rsrc_offset 0x01
			set sub_rsrc_type 0x00
			set human_category ""

			# Loop over the sResources in this entry
			while {[expr $sub_rsrc_offset != 0x000000 && $sub_rsrc_type != 0xFF]} {
				section -collapsed "sRsrc"
				section -collapsed "Metadata"
				set sub_rsrc_type [uint8 "Type"]
				set sub_rsrc_offset [int24 "Offset/Raw Data"]
				endsection
				set reset_location [pos]
				move -4
				# We are now at the proper location to add $sub_rsrc_offsets.
				# The location will be reset automatically after the switch to continue looping.

				# TODO: Sometimes this error handling doesn't pop us out enough sections
				set status [catch {
					# See NuBus documentation, page 164 (205), also page 185 (226)
					switch -glob $sub_rsrc_type {
						1 {
							# See NuBus documentation, page 165 (206)
							sectionname "sRsrcType (1)"
							move $sub_rsrc_offset
							# TODO: remove bit 31: "bit 31 is reserved for Apple's use" -- page 145
							set category [uint16]
							move -2
							set human_category [rsrc_type $category]
							entry "Category" "$human_category ($category)" 2
							move 2
							set ctype [uint16]
							move -2
							entry "cType" [resolve_ctype $category $ctype] 2
							move 2
							set drSW [uint16]
							move -2
							entry "DrSW" [resolve_drsw $drSW] 2
							move 2
							# This *should* be non-unique across vendors, so we can't do anything but list the number
							uint16 "DrHW"
						}
						2 {
							sectionname "sRsrcName (2)"
							move $sub_rsrc_offset
							set rsrc_name [cstr "macroman" "Name"]
						}
						3 {
							sectionname "sRsrcIcon (3)"
							# TODO: Is this the correct offset?
							move $sub_rsrc_offset
							# Size per cards documentation, page 184 (225)
							bytes 128 "Icon"
						}
						4 {
							sectionname "sRsrcDrvrDir (4)"
							driver_dir $sub_rsrc_offset
						}
						5 {
							# Card page 167
							sectionname "sRsrcLoadRec (5)"
							exec_block $sub_rsrc_offset
						}
						6 {
							# Card page 168
							sectionname "sRsrcBootRec (6)"
							exec_block $sub_rsrc_offset
						}
						7 {
							sectionname "sRsrcFlags (7)"
							move 2
							# TODO: is that "sResource flags for sRsrc_Flags" in ROMDefs.h?
							# Card manual page page 169
							uint16_bits 1 "fOpenAtStart"
							move -2
							uint16_bits 2 "f32BitMode"
						}
						8 {
							sectionname "sRsrcHWDevld (8)"
							move 1
							entry "Hardware Device ID" $sub_rsrc_offset 3
						}
						10 {
							sectionname "MinorBaseOS (10)"
							move $sub_rsrc_offset
							uint32 "minBaseOS"
						}
						11 {
							sectionname "MinorLength (11)"
							move $sub_rsrc_offset
							uint32 "minorLength"
						}
						12 {
							sectionname "MajorBaseOS (12)"
							move $sub_rsrc_offset
							uint32 "majBaseOS"
						}
						13 {
							sectionname "MajorLength (13)"
							move $sub_rsrc_offset
							uint32 "majorLength"
						}
						14 {
							# from ROMDefs.h: sBlock diagnostic code
							sectionname "sRsrcTest (14)"
							entry "TODO" 0
							# TODO: Parse
						}
						15 {
							sectionname "sRsrcCicn (15)"
							entry "TODO" 0
							# Card documentation page 185 (226)
							# TODO: Parse
						}
						16 {
							# Card documentation page 170 (211)
							sectionname "sRsrclcl8 (16)"
							move $sub_rsrc_offset
							# Equivalent to icl8 at 32x32 so fixed at 1024
							bytes 1024 "Icon"
						}
						17 {
							# Card documentation page 171 (212)
							sectionname "sRsrclcl4 (17)"
							move $sub_rsrc_offset
							# Equivalent to icl4 at 32x32 so fixed at 512
							bytes 512 "Icon"
						}
						20 {
							# From ROMDefs.h: general slot block xfer info
							# Card book page 181 (222)
							sectionname "sBlockTransferInfo (20)"
							block_transfer_info $sub_rsrc_offset
						}
						21 {
							# From ROMDefs.h: slot max. locked xfer count
							sectionname "sMaxLockedTransferCount (21)"
							move $sub_rsrc_offset
							uint32 "Maximum Locked Transfers"
						}
						32 {
							sectionname "BoardID (32)"
							move 1
							entry "Board ID" [expr $sub_rsrc_offset & 0xFF] 3
						}
						33 {
							sectionname "PRAMInitData (33)"
							# TODO: Might be broken
							move $sub_rsrc_offset
							move 1
							uint24 "Physical Block Size"
							move 2
							uint8 "Byte 1"
							uint8 "Byte 2"
							uint8 "Byte 3"
							uint8 "Byte 4"
							uint8 "Byte 5"
							uint8 "Byte 6"
						}
						34 {
							sectionname "PrimaryInit (34)"
							exec_block $sub_rsrc_offset
						}
						35 {
							sectionname "STimeOut (35)"
							move 1
							uint24 "Time Out"
						}
						36 {
							sectionname "VendorInfo (36)"
							set vendorid [vendor_info $sub_rsrc_offset]
						}
						37 {
							# TODO: Confirm
							# From ROMDefs.h: Board Flags
							sectionname "BoardFlags (37)"
							entry "TODO" 0
							# TODO: Parse
						}
						38 {
							sectionname "SecondaryInit (38)"
							exec_block $sub_rsrc_offset
						}
						64 {
							sectionname "sGammaDir (54)"
							gamma_dir $sub_rsrc_offset
						}
						65 {
							sectionname "sRsrcVidNames (65)"
							vid_names $sub_rsrc_offset
						}
						80 {
							# From ROMDefs.h: spID for Docking Handlers
							sectionname "sRsrcDock (80)"
							entry "TODO" 0
							# TODO: Parse
						}
						85 {
							# From ROMDefs.h: spID for board diagnostics
							sectionname "sDiagRec (85)"
							entry "TODO" 0
							# TODO: Parse
						}
						108 {
							sectionname "sMemory (108)"
							smemory $sub_rsrc_offset
						}
						123 {
							# From ROMDefs.h: more video info for Display Manager -- timing information
							sectionname "sVidAuxParams (123)"
							vid_aux_params $sub_rsrc_offset
						}
						124 {
							# From ROMDefs.h: DatLstEntry for debuggers indicating video anamolies
							sectionname "sDebugger (124)"
							entry "TODO" 0
							# TODO: Parse
						}
						125 {
							# From ROMDefs.h: video attributes data field (optional,word)
							sectionname "sVidAttributes (125)"
							move 2
							# fLCDScreen bit 0 - when set is LCD, else is CRT
							uint16_bits 0 "fLCDScreen"
							move -2
							# fBuiltInDisplay 1 - when set is built-in (in the box) display, else not
							uint16_bits 1 "fBuiltInDisplay"
							move -2
							# fDefaultColor 2 - when set display prefers multi-bit color, else gray
							uint16_bits 2 "fDefaultColor"
							move -2
							# fActiveBlack 3 - when set black on display must be written, else display is naturally black
							uint16_bits 3 "fActiveBlack"
							move -2
							# fDimMinAt1 4 - when set should dim backlight to level 1 instead of 0
							uint16_bits 4 "fDimAt1"
							# TODO.....two 4th bits???
							# fBuiltInDetach 4 - when set is built-in (in the box), but detaches
						}
						126 {
							# From ROMDefs.h
							# From card docs page 186
							# TODO: No, it's not on page 186?
							sectionname "sVidParmDir (126)"
							svidparam_dir $sub_rsrc_offset
						}
						140 {
							# From ROMDefs.h: directory of backlight tables
							sectionname "sBkltParmDir (140)"
							entry "TODO" 0
							# TODO: Parse
						}
						2* {
							# TODO: This whole section layout is kinda gross
							switch -regexp $sub_rsrc_type {
								{(20[0-9]|2[1-4][0-9])} {
									# Special timing data for SuperMac cards
									# TODO: This misparses early cards which appear to use a different format in a different set of sResources
									if {$vendorid == "SuperMac Technology"} {
										sectionname "SuperMac Timing ($sub_rsrc_type)"
										move $sub_rsrc_offset
										uint32 "Length"
										uint8 "Clock"
										bytes 3 "Unknown"
										bytes 8 "Unknown BSR Data"
										uint16 "Horizontal End Sync"
										uint16 "Horizontal End Blank"
										uint16 "Horizontal Start Blank"
										uint16 "Horizontal Total"
										uint16 "Vertical End Sync"
										uint16 "Vertical End Blank"
										uint16 "Vertical Start Blank"
										uint16 "Vertical Total"
										bytes 44 "Unknown SMT Data"
										bytes 6 "Unknown SQD Data"
										uint16 "Horizontal Resolution"
										uint16 "Vertical Resolution"
										uint8 "sRsrc ID"
										bytes 1 "Unknown"
										set timing_name [cstr macroman "Name"]
										sectionname "SuperMac Timing \[$timing_name\] ($sub_rsrc_type)"
									} else {
										sectionname "Unknown ($sub_rsrc_type)"
									}
								}
								255 {
									sectionname "Terminator (255)"
								}
								default {
									sectionname "Unknown ($sub_rsrc_type)"
								}
							}
						}
						default {
							sectionname "Unknown ($sub_rsrc_type)"
						}
					}
					# Process the special types for certain devices
					if {$sub_rsrc_type >= 128 && $sub_rsrc_type != 255} {
						if {$category == 3 && $drSW == 1} {
							# TODO: It's possible other software types use this same storage, but type 1 indicates: "For example, under Category Display and cType Video atypical predefined driver software interface would be one defined by Apple to work with QuickDraw using the Macintosh Operating System frame buffers." -- page 146
							sectionname "Video Mode ($sub_rsrc_type)"
							# TODO: We should check this everywhere, but for now this is a quick fix for the Futura SX ROM
							if {[valid_rsrc_dir_offset $sub_rsrc_offset] == 0} {
								sectionvalue "ERROR"
								entry "ERROR" "Irrational offset: $sub_rsrc_offset"
							} else {
								set vid_bounds [vid_mode $sub_rsrc_offset]
								# TODO: Redundantly setting part of the section name
								sectionname "Video Mode \[$vid_bounds\] ($sub_rsrc_type)"
							}
						} elseif {$category == 4 && $ctype == 1 && $sub_rsrc_type == 128} {
							# TODO: This only seems to apply for resource type 128...what do the others do?
							sectionname "Ethernet Address ($sub_rsrc_type)"
							move $sub_rsrc_offset
							# Compute 48-bit address
							set first [uint32]
							set second [uint16]
							set mac [expr $first << 16 | $second]
							move -6
							entry "MAC" [format 0x%010x $mac] 6
						} elseif {$category == 10} {
							# ROMDefs.inc references these for CPU sResources
							# TODO: Is this really right?
							switch $sub_rsrc_type {
								129 {
									sectionname "MajRAMSp (128)"
									entry "TODO" 0
								}
								130 {
									sectionname "MinROMSp (129)"
									entry "TODO" 0
								}
							}
						}
					}
				} err]
				if {$status} {
					sectionvalue "ERROR"
					entry "ERROR" $err 1
					# TODO: Sometimes we haven't started a section when we error which causes us to crash anyway -- need to fix that we don't over-close sections
					#endsection
				}
				goto $reset_location
				endsection
			}
			if {$rsrc_type < 127} {
				set human_category "Board"
			}
			if {$rsrc_name != ""} {
				sectionname "$human_category \[$rsrc_name\] ($rsrc_type)"
			} else {
				sectionname "$human_category ($rsrc_type)"
			}
			goto $location
		}
		endsection
	}
}

#### Universal Tables parser

proc product_pa6pb3 {input} {
	if { 0 } {
	} elseif { $input == 0x00000000 } { set result "Mac IIx"
	} elseif { $input == 0x00000008 } { set result "Mac II"
	} elseif { $input == 0x40000000 } { set result "Mac SE/30"
	} elseif { $input == 0x40000008 } { set result "Mac IIcx"
	} else {
		set result [format "Unknown (0x%X)" $input]
	}
	return $result
}

proc product_via_pa6421 {input} {
	if { 0 } {
	} elseif { $input == 0x00000000 } { set result "PowerBook 150"
	} elseif { $input == 0x02000000 } { set result "Color Classic"
	} elseif { $input == 0x04000000 } { set result "Unused"
	} elseif { $input == 0x06000000 } { set result "Unreleased MDU-using SE/30 Successor"
	} elseif { $input == 0x10000000 } { set result "Quadra 950"
	} elseif { $input == 0x12000000 } { set result "PowerBook 140/170, Classic II, Quadra 800"
	} elseif { $input == 0x14000000 } { set result "Used, unknown machine"
	} elseif { $input == 0x16000000 } { set result "Mac IIsi"
	} elseif { $input == 0x40000000 } { set result "Quadra 700, Centris 610"
	} elseif { $input == 0x42000000 } { set result "Unreleased 20Mhz 650"
	} elseif { $input == 0x44000000 } { set result "Quadra 610"
	} elseif { $input == 0x46000000 } { set result "Mac IIci, Centris (25Mhz) 650"
	} elseif { $input == 0x50000000 } { set result "Quadra 900"
	} elseif { $input == 0x52000000 } { set result "Mac IIfx, Quadra (33Mhz) 650"
	} elseif { $input == 0x54000000 } { set result "Mac LC, LC2, IIvx, IIvi"
	} elseif { $input == 0x56000000 } { set result "Mac IIci w/PGC"
	} else {
		set result [format "Unknown (0x%X)" $input]
	}
	return $result
}

proc productinfo_name {boxnumber DecoderKind viamask viaid cpuid} {
	if { 0 } {
	} elseif { $boxnumber ==  14 } { set result "InfoQuadra900"
	} elseif { $boxnumber ==  15 } { set result "InfoPowerBook170"
	} elseif { $boxnumber ==  16 } { set result "InfoQuadra700"
	} elseif { $boxnumber ==  20 } { set result "InfoQuadra950"
	} elseif { $boxnumber ==  21 } { set result "InfoLCIII"
	} elseif { $boxnumber ==  23 } { set result "InfoPowerBookDuo210"
	} elseif { $boxnumber ==  24 } { set result "InfoCentris650"
	} elseif { $boxnumber ==  26 } { set result "InfoPowerBookDuo230"
	} elseif { $boxnumber ==  27 } { set result "InfoPowerBook180"
	} elseif { $boxnumber ==  29 } { set result "InfoQuadra800"
	} elseif { $boxnumber ==  30 } { set result "InfoQuadra650"
	} elseif { $boxnumber ==  31 } { set result "InfoMacLC"
	} elseif { $boxnumber ==  32 } { set result "InfoPowerBookDuo235"
	} elseif { $boxnumber ==  34 } { set result "InfoVail16"
	} elseif { $boxnumber ==  37 } { set result "InfoCyclone33"
	} elseif { $boxnumber ==  45 } { set result "InfoWombat40"
	} elseif { $boxnumber ==  46 } { set result "InfoCentris610"
	} elseif { $boxnumber ==  47 } { set result "InfoQuadra610"
	} elseif { $boxnumber ==  52 } { set result "InfoWombat20"
	} elseif { $boxnumber ==  53 } { set result "InfoWombat40F"
	} elseif { $boxnumber ==  54 } { set result "InfoCentris660AV"
	} elseif { $boxnumber ==  55 && $cpuid == 0x3235 } { set result "InfoRiscQuadra700"
	} elseif { $boxnumber ==  56 } { set result "InfoVail33"
	} elseif { $boxnumber ==  57 } { set result "InfoWLCD33"
	} elseif { $boxnumber ==  58 } { set result "InfoPDMcoldFusion"
	} elseif { $boxnumber ==  61 } { set result "InfoTNTProto1"
	} elseif { $boxnumber ==  66 } { set result "InfoBlackbird"
	} elseif { $boxnumber ==  69 } { set result "InfoPDM"
	} elseif { $boxnumber ==  72 } { set result "InfoQuadra840AV"
	} elseif { $boxnumber ==  73 } { set result "InfoTempest33"
	} elseif { $boxnumber ==  76 && $cpuid == 0x3200} { set result "InfoRiscCentris650"
	} elseif { $boxnumber ==  76 && $cpuid == 0x3201} { set result "InfoRiscQuadra800"
	} elseif { $boxnumber ==  76 && $cpuid == 0x3202} { set result "InfoRiscQuadra610"
	} elseif { $boxnumber ==  76 && $cpuid == 0x3203} { set result "InfoRiscQuadra650"
	} elseif { $boxnumber ==  76 && $cpuid == 0x3204} { set result "InfoRiscCentris610"
	} elseif { $boxnumber ==  97 } { set result "InfoYeager"
	} elseif { $boxnumber ==  98 } { set result "InfoRiscQuadra900"
	} elseif { $boxnumber ==  99 } { set result "InfoRiscQuadra950"
	} elseif { $boxnumber == 106 } { set result "InfoPDMCarlSagan"
	} elseif { $boxnumber == 110 } { set result "InfoSTPQuadra700"
	} elseif { $boxnumber == 111 } { set result "InfoSTPQuadra900"
	} elseif { $boxnumber == 112 } { set result "InfoSTPQuadra950"
	} elseif { $boxnumber == 113 } { set result "InfoSTPCentris610"
	} elseif { $boxnumber == 114 } { set result "InfoSTPCentris650"
	} elseif { $boxnumber == 115 } { set result "InfoSTPQuadra610"
	} elseif { $boxnumber == 116 } { set result "InfoSTPQuadra650"
	} elseif { $boxnumber == 117 && $viamask == 0x56000000 && $viaid == 0x12000000 } { set result "InfoSTPQuadra800"
	} elseif { $boxnumber == 117 && $viamask == 0x56000000 && $viaid == 0x16000000 } { set result "InfoSTPQuadra40F"
	} elseif { $boxnumber == 253 && $DecoderKind ==  0 } { set result "InfoUnknownUnknown"
	} elseif { $boxnumber == 253 && $DecoderKind ==  5 } { set result "InfoMDUUnknown"
	} elseif { $boxnumber == 253 && $DecoderKind ==  6 } { set result "InfoOSSUnknown"
	} elseif { $boxnumber == 253 && $DecoderKind ==  7 } { set result "InfoVISAUnknown"
	} elseif { $boxnumber == 253 && $DecoderKind ==  9 } { set result "InfoJAWSUnknown"
	} elseif { $boxnumber == 253 && $DecoderKind == 12 } { set result "InfoNiagraUnknown"
	} else { set result ""}
	return $result;
}

proc product_cpuid {input} {
	if { 0 } {
	} elseif { $input == 0      } { set result "0"
	} elseif { $input == 0x0001 } { set result "LC III (cpuIDHiVol|cpuIDinReg|Vail25IDField)"
	} elseif { $input == 0x0003 } { set result "LC III+ (cpuIDHiVol|cpuIDinReg|Vail33IDField)"
	} elseif { $input == 0x0100 } { set result "LC 520 (cpuIDHiVol|\$100)"
	} elseif { $input == 0x0101 } { set result "LC 550 (and Color Classic II?) (cpuIDHiVol|\$101)"
	} elseif { $input == 0x1000 } { set result "PowerBook Duo 280c (cpuIDPortable|cpuIDinReg|0)"
	} elseif { $input == 0x1002 } { set result "PowerBook Duo 270c (cpuIDPortable|cpuIDinReg|2)"
	} elseif { $input == 0x1004 } { set result "PowerBook Duo 210 (cpuIDPortable|cpuIDinReg|4)"
	} elseif { $input == 0x1005 } { set result "PowerBook Duo 230 (cpuIDPortable|cpuIDinReg|5)"
	} elseif { $input == 0x1006 } { set result "PowerBook Duo 235 (cpuIDPortable|cpuIDinReg|6)"
	} elseif { $input == 0x1808 } { set result "PowerBook 520/540 (cpuIDPortable|cpuIDinBoard|8)"
	} elseif { $input == 0x1809 } { set result "PowerBook Duo 2300 (cpuIDPortable|cpuIDinBoard|9)"
	} elseif { $input == 0x180A } { set result "PowerBook 5300 (cpuIDPortable|cpuIDinBoard|10)"
	} elseif { $input == 0x180B } { set result "PowerBook 190 (cpuIDPortable|cpuIDinBoard|11)"
	} elseif { $input == 0x2015 } { set result "IIvx (cpuIDHiEnd|cpuIDinReg|21)"
	} elseif { $input == 0x2221 } { set result "LC 475 (cpuIDHiEnd|\$221)"
	} elseif { $input == 0x2225 } { set result "Quadra 605 (cpuIDHiEnd|\$225)"
	} elseif { $input == 0x2226 } { set result "Quadra 630 (cpuIDHiEnd|\$226)"
	} elseif { $input == 0x222E } { set result "LC 575 (cpuIDHiEnd|\$22E)"
	} elseif { $input == 0x2830 } { set result "Quadra 660/840 (cpuIDHiEnd|cpuIDinMMC|\$30)"
	} elseif { $input == 0x2BAD } { set result "Quadra/Centris 610/650/800 (cpuIDHiEnd|cpuIDinVIA|\$3AD)"
	} elseif { $input == 0x3010 } { set result "PowerMac 6100 (cpuIDRISC|\$3010)"
	} elseif { $input == 0x3011 } { set result "PDM (cpuIDRISC|cpuIDinReg|\$3011)"
	} elseif { $input == 0x3012 } { set result "PowerMac 7100 (cpuIDRISC|cpuIDinReg|\$3012)"
	} elseif { $input == 0x3013 } { set result "PowerMac 8100 (cpuIDRISC|cpuIDinReg|\$3013)"
	} elseif { $input == 0x3020 } { set result "PowerMac 7500 (cpuIDRISC|cpuIDinReg|\$3020)"
	} elseif { $input == 0x3021 } { set result "PowerMac 7300 (cpuIDRISC|\$3021)"
	} elseif { $input == 0x3022 } { set result "PowerMac 7600/8600/9600 (cpuIDRISC|\$3022)"
	} elseif { $input == 0x3025 } { set result "PowerBook 2400 (cpuIDRISC|\$3025)"
	} elseif { $input == 0x3026 } { set result "PowerBook 3400 (cpuIDRISC|\$3026)"
	} elseif { $input == 0x3041 } { set result "PowerMac G3 “Beige” (cpuIDRISC|\$3041)"
	} elseif { $input == 0x3042 } { set result "PowerBook G3 “WallStreet” (cpuIDRISC|\$3042)"
	} elseif { $input == 0x3046 } { set result "PowerBook G3 “WallStreet” (cpuIDRISC|\$3046)"
	} elseif { $input == 0x3200 } { set result "Wombat product table w/Smurf...a centris650 (Lego plastics) (cpuIDRISC|\$1200)"
	} elseif { $input == 0x3201 } { set result "Quadra800 with a Risc card (cpuIDRISC|\$1201)"
	} elseif { $input == 0x3202 } { set result "Quadra610 with a smurf card (cpuIDRISC|\$1202)"
	} elseif { $input == 0x3203 } { set result "Quadra650 with a smurf card (cpuIDRISC|\$1203)"
	} elseif { $input == 0x3204 } { set result "Centris610 with a Smurf card (cpuIDRISC|\$1204)"
	} elseif { $input == 0x3235 } { set result "601 Smurf card running in Quadra 700 (cpuIDRISC|\$1235)"
	} elseif { $input == 0x3236 } { set result "601 Smurf card running in Quadra 900 (cpuIDRISC|\$1236)"
	} elseif { $input == 0x3237 } { set result "601 Smurf card running in Quadra 950 (cpuIDRISC|\$1237)"
	} elseif { $input == 0x7100 } { set result "Pippin @Mark ((4<<12)|cpuIDRISC|\$100)"
	} else {
		set result [format "Unknown (0x%X)" $input]
	}
	return $result
}

proc product_kind {input} {
	switch $input {
		253 { set result "boxUnknown" }
		254 { set result "boxPlus" }
		255 { set result "boxSE" }
		  0 { set result "boxMacII" }
		  1 { set result "boxMacIIx" }
		  2 { set result "boxMacIIcx" }
		  3 { set result "boxSE30" }
		  4 { set result "boxPortable" }
		  5 { set result "boxMacIIci" }
		  6 { set result "boxFourSquare" }
		  7 { set result "boxMacIIfx" }
		  8 { set result "boxAuroraCX16" }
		  9 { set result "boxAuroraSE25" }
		 10 { set result "boxAuroraSE16" }
		 11 { set result "boxMacClassic" }
		 12 { set result "boxMacIIsi" }
		 13 { set result "boxMacLC" }
		 14 { set result "boxQuadra900" }
		 15 { set result "boxPowerBook170" }
		 16 { set result "boxQuadra700" }
		 17 { set result "boxClassicII" }
		 18 { set result "boxPowerBook100" }
		 19 { set result "boxPowerBook140" }
		 20 { set result "boxQuadra950" }
		 21 { set result "boxLCIII" }
		 22 { set result "boxSoftmacSUN" }
		 23 { set result "boxPowerBookDuo210" }
		 24 { set result "boxCentris650" }
		 25 { set result "boxColumbia" }
		 26 { set result "boxPowerBookDuo230" }
		 27 { set result "boxPowerBook180" }
		 28 { set result "boxPowerBook160" }
		 29 { set result "boxQuadra800" }
		 30 { set result "boxQuadra650" }
		 31 { set result "boxMacLCII" }
		 32 { set result "boxPowerBookDuo250" }
		 33 { set result "boxDBLite20" }
		 34 { set result "boxVail16" }
		 35 { set result "boxCarnation25" }
		 36 { set result "boxCarnation16" }
		 37 { set result "boxCyclone33" }
		 38 { set result "boxBrazil16L" }
		 39 { set result "boxBrazil32L" }
		 40 { set result "boxBrazil16F" }
		 41 { set result "boxBrazil32F" }
		 42 { set result "boxBrazilC" }
		 43 { set result "boxSlice" }
		 44 { set result "boxMonet" }
		 45 { set result "boxWombat40" }
		 46 { set result "boxCentris610" }
		 47 { set result "boxQuadra610" }
		 48 { set result "boxPowerBook145" }
		 49 { set result "boxBrazil32cF" }
		 50 { set result "boxHook" }
		 51 { set result "boxUnused" }
		 52 { set result "boxWombat20" }
		 53 { set result "boxWombat40F" }
		 54 { set result "boxCentris660AV" }
		 55 { set result "boxPDM|boxRiscQuadra700" }
		 56 { set result "boxVail33" }
		 57 { set result "boxWLCD33" }
		 58 { set result "boxPDM66F" }
		 59 { set result "boxPDM80F" }
		 60 { set result "boxPDM100F" }
		 61 { set result "boxTNTProto1" }
		 62 { set result "boxTesseractF" }
		 63 { set result "boxTesseractC" }
		 64 { set result "boxJust930" }
		 65 { set result "boxHokusai" }
		 66 { set result "boxBlackbird" }
		 67 { set result "boxBlackbirdLC" }
		 68 { set result "boxPDMEvt1" }
		 69 { set result "boxPDM50WLCD" }
		 70 { set result "boxYeagerFSTN" }
		 71 { set result "boxPowerBookDuo270C" }
		 72 { set result "boxQuadra840AV" }
		 73 { set result "boxTempest33" }
		 74 { set result "boxHook33" }
		 75 { set result "boxSlice25" }
		 76 { set result "boxRiscCentris650" }
		 77 { set result "boxSlice33" }
		 78 { set result "boxNorad" }
		 79 { set result "boxBudMan" }
		 80 { set result "boxPrimus20" }
		 81 { set result "boxOptimus20" }
		 82 { set result "boxHookTV" }
		 83 { set result "boxLC475" }
		 84 { set result "boxPrimus33" }
		 85 { set result "boxOptimus25" }
		 86 { set result "boxLC575" }
		 87 { set result "boxAladdin20" }
		 88 { set result "boxQuadra605" }
		 89 { set result "boxAladdin33" }
		 90 { set result "boxMalcolm25" }
		 91 { set result "boxMalcolm33" }
		 92 { set result "boxSlimus25" }
		 93 { set result "boxSlimus33" }
		 94 { set result "boxPDM66WLCD" }
		 95 { set result "boxPDM80WLCD" }
		 96 { set result "boxYeagerG" }
		 97 { set result "boxYeagerC" }
		 98 { set result "boxRiscQuadra900" }
		 99 { set result "boxRiscQuadra950" }
		100 { set result "boxRiscCentris610" }
		101 { set result "boxRiscQuadra800" }
		102 { set result "boxRiscQuadra610" }
		103 { set result "boxRiscQuadra650" }
		104 { set result "boxRiscTempest" }
		105 { set result "boxPDM50L" }
		106 { set result "boxPDM66L" }
		107 { set result "boxPDM80L" }
		108 { set result "boxBlackbirdBFD" }
		109 { set result "boxJedi" }
		110 { set result "boxSTPQ700" }
		111 { set result "boxSTPQ900" }
		112 { set result "boxSTPQ950" }
		113 { set result "boxSTPC610" }
		114 { set result "boxSTPC650" }
		115 { set result "boxSTPQ610" }
		116 { set result "boxSTPQ650" }
		117 { set result "boxSTPQ800" }
		118 { set result "boxAJ" }
		119 { set result "boxAJ80" }
		120 { set result "boxMalcolmBB" }
		121 { set result "boxMalcolmBB80" }
		122 { set result "boxM2" }
		123 { set result "boxM280" }
		124 { set result "boxSoftmacHP" }
		125 { set result "boxSoftmacIBM" }
		126 { set result "boxSoftmacAUX" }
		127 { set result "boxExtended" }
		default { set result "Unknown ($input)" }
	}
	return $result
}

proc decoder_kind {input} {
	switch $input {
		 0 { set result "UnknownDecoder" }
		 1 { set result "MacPALDecoder" }
		 2 { set result "BBUDecoder" }
		 3 { set result "NormandyDecoder" }
		 4 { set result "GLUEDecoder" }
		 5 { set result "MDUDecoder" }
		 6 { set result "OSSFMCDecoder" }
		 7 { set result "VISADecoder" }
		 8 { set result "OrwellDecoder" }
		 9 { set result "JAWSDecoder" }
		10 { set result "MSCDecoder" }
		11 { set result "SonoraDecoder" }
		12 { set result "NiagraDecoder" }
		13 { set result "YMCADecoder" }
		14 { set result "djMEMCDecoder" }
		15 { set result "HMCDecoder" }
		16 { set result "PrattDecoder" }
		17 { set result "HHeadDecoder" }
		default { set result "Unknown ($input)" }
	}
	return $result
}

proc rom85_word {input} {
	switch $input {
		16383 { set result "New ROMs, Power Off ability, ColorQD" }
		32767 { set result "New ROMs, no Power Off ability, no Color QD" }
		65535 { set result "not New ROM, no Power Off ability, no Color QD" }
		default { set result "Unknown ($input)" }
	}
	return $result
}

proc default_rsrcs {input} {
	switch $input {
		1 { set result "AppleTalk1" }
		2 { set result "AppleTalk2" }
		3 { set result "AppleTalk2_NetBoot_FPU" }
		4 { set result "AppleTalk2_NetBoot_NoFPU" }
		default { set result "Unknown ($input)" }
	}
	return $result
}

proc productinfo_vers {input} {
	switch $input {
		1 { set result "ProductInfoVersion" }
		default { set result "Unknown ($input)" }
	}
	return $result
}

proc decoderinfo_vers {input} {
	switch $input {
		1 { set result "DecoderInfoVersion" }
		default { set result "Unknown ($input)" }
	}
	return $result
}

proc adb_mask {input} {
	switch $input {
		0 { set result "ADBXcvr" }
		1 { set result "ADBPwrMgr" }
		2 { set result "ADBIop" }
		3 { set result "ADBEgret" }
		4 { set result "ADBspare4" }
		5 { set result "ADBspare5" }
		6 { set result "ADBspare6" }
		7 { set result "ADBspare7" }
		default { set result "Unknown ($input)" }
	}
	return $result
}

proc clock_mask {input} {
	switch $input {
		0 { set result "ClockRTC" }
		1 { set result "ClockPwrMgr" }
		2 { set result "ClockEgret" }
		3 { set result "ClockNoPram" }
		4 { set result "Clockspare4" }
		5 { set result "Clockspare5" }
		6 { set result "Clockspare6" }
		7 { set result "Clockspare7" }
		default { set result "Unknown ($input)" }
	}
	return $result
}

proc keysw_mask {input} {
	switch $input {
		0 { set result "KeyswNone" }
		1 { set result "KeyswCaboose" }
		2 { set result "KeyswSpare2" }
		3 { set result "KeyswSpare3" }
		default { set result "Unknown ($input)" }
	}
	return $result
}

proc egretfw_mask {input} {
	switch $input {
		0 { set result "EgretNone" }
		1 { set result "Egret8" }
		2 { set result "Caboose" }
		3 { set result "Cuda" }
		4 { set result "EgretFWSpare4" }
		5 { set result "EgretFWSpare5" }
		6 { set result "EgretFWSpare6" }
		7 { set result "EgretFWSpare7" }
		default { set result "Unknown ($input)" }
	}
	return $result
}

proc shal_mask {input} {
	switch $input {
		0 { set result "SHALReserved" }
		1 { set result "SHALPSC" }
		2 { set result "SHALAMIC" }
		3 { set result "SHALSpare3" }
		4 { set result "SHALSpare4" }
		5 { set result "SHALSpare5" }
		6 { set result "SHALSpare6" }
		7 { set result "SHALSpare7" }
		default { set result "Unknown ($input)" }
	}
	return $result
}

proc one_addr {thebits bitnumber name} {
	#entry "what" [format "bitnumber:%d len:%d thebits:%s" $bitnumber [string length $thebits] $thebits] 4
	set addr [uint32]
	if {$bitnumber < [string length $thebits]} {
		set thebit [string index $thebits $bitnumber]
		if {$addr != 0} {
			move -4
			if {$thebit == "1"} {
				uint32 -hex $name
			} else {
				entry $name [format "0x%X (but not marked valid)" $addr] 4
				move 4
			}
		} elseif {$thebit == "1"} {
			entry $name [format "0x%X (but marked valid)" $addr] 4
			move 4
		}
	}
}

proc one_bit {bit name} {
	set val [uint32_bits $bit]
	move -4
	if {$val != 0} {
		uint32_bits $bit $name
		move -4
	}
}

proc one_bit16 {bit name} {
	set val [uint16_bits $bit]
	move -2
	if {$val != 0} {
		uint16_bits $bit $name
		move -2
	}
}

proc parse_flags {base ext} {
	set BasesValid [uint32]
	set BasesValid1 [uint32]
	set BasesValid2 [uint32]
	set ExtValid [uint32]
	set ExtValid1 [uint32]
	set ExtValid2 [uint32]
	move -24

	if {$BasesValid != 0} {
		section $base {
			one_bit  0 "ROMExists"
			one_bit  1 "DiagROMExists"
			one_bit  2 "VIA1Exists"
			one_bit  3 "SCCRdExists"
			one_bit  4 "SCCWrExists"
			one_bit  5 "IWMExists"
			one_bit  6 "PWMExists"
			one_bit  7 "SoundExists"
			one_bit  8 "SCSIExists"
			one_bit  9 "SCSIDackExists"
			one_bit 10 "SCSIHskExists"
			one_bit 11 "VIA2Exists"
			one_bit 12 "ASCExists"
			one_bit 13 "RBVExists"
			one_bit 14 "VDACExists"
			one_bit 15 "SCSIDMAExists"
			one_bit 16 "SWIMIOPExists"
			one_bit 17 "SCCIOPExists"
			one_bit 18 "OSSExists"
			one_bit 19 "FMCExists"
			one_bit 20 "RPUExists"
			one_bit 21 "OrwellExists"
			one_bit 22 "JAWSExists"
			one_bit 23 "SonicExists"
			one_bit 24 "SCSI96_1Exists"
			one_bit 25 "SCSI96_2Exists"
			one_bit 26 "DAFBExists"
			one_bit 27 "PSCExists"
			one_bit 28 "ROMPhysExists"
			one_bit 29 "PatchROMExists"
			one_bit 30 "NewAgeExists"
			one_bit 31 "Unused31Exists"
			move 4
		}
	} else {
		uint32 $base
	}

	if {$BasesValid1 != 0} {
		section [format "%s1" $base] {
			one_bit  0 "SingerExists"
			one_bit  1 "DSPExists"
			one_bit  2 "MACEExists"
			one_bit  3 "MUNIExists"
			one_bit  4 "AMICExists"
			one_bit  5 "PrattExists"
			one_bit  6 "SWIM3Exists"
			one_bit  7 "AwacsExists"
			one_bit  8 "CivicExists"
			one_bit  9 "SebastianExists"
			one_bit 10 "BARTExists"
			one_bit 11 "GrandCentralExists"
			one_bit 12 "PBX1Exists"
			one_bit 13 "PBX2Exists"
			one_bit 14 "PBX3Exists"
			one_bit 15 "ATAExists"
			one_bit 16 "HammerHeadExists"
			one_bit 17 "PlatinumExists"
			one_bit 18 "Pratt2Exists"
			one_bit 19 "PSXExists"
			one_bit 20 "OHareExists"
			one_bit 21 "GrackleExists"
			one_bit 22 "HydraExists"
			one_bit 23 "SuperIOExists"
			one_bit 24 "SIOExists"
			one_bit 25 "HeathrowExists"
			one_bit 26 "MFMFloppyExists"
			one_bit 27 "MFMMethodsVectorExists"
			one_bit 28 "FatManExists"
			one_bit 29 "OpenPICExists"
			one_bit 30 "CHRPNess"
			one_bit 31 "GatwickExists"
			move 4
		}
	} else {
		uint32 [format "%s1" $base]
	}

	if {$BasesValid2 != 0} {
		section [format "%s2" $base] {
			one_bit  0 "Unused64Exists"
			one_bit  1 "BarExists"
			one_bit  2 "Unused66Exists"
			one_bit  3 "Unused67Exists"
			one_bit  4 "Unused68Exists"
			one_bit  5 "Unused69Exists"
			one_bit  6 "Unused70Exists"
			one_bit  7 "Unused71Exists"
			one_bit  8 "Unused72Exists"
			one_bit  9 "Unused73Exists"
			one_bit 10 "Unused74Exists"
			one_bit 11 "Unused75Exists"
			one_bit 12 "Unused76Exists"
			one_bit 13 "Unused77Exists"
			one_bit 14 "Unused78Exists"
			one_bit 15 "Unused79Exists"
			one_bit 16 "Unused80Exists"
			one_bit 17 "Unused81Exists"
			one_bit 18 "Unused82Exists"
			one_bit 19 "Unused83Exists"
			one_bit 20 "Unused84Exists"
			one_bit 21 "Unused85Exists"
			one_bit 22 "Unused86Exists"
			one_bit 23 "Unused87Exists"
			one_bit 24 "Unused88Exists"
			one_bit 25 "Unused89Exists"
			one_bit 26 "Unused90Exists"
			one_bit 27 "Unused91Exists"
			one_bit 28 "Unused92Exists"
			one_bit 29 "Unused93Exists"
			one_bit 30 "Unused94Exists"
			one_bit 31 "Unused95Exists"
			move 4
		}
	} else {
		uint32 [format "%s2" $base]
	}

	if {$ExtValid != 0} {
		section $ext {
			one_bit 0 "PGCInstalled"

			set ADBMask [uint32_bits 3,2,1]
			move -4
			entry "ADBMask" [adb_mask $ADBMask] 4

			set ClockMask [uint32_bits 6,5,4]
			move -4
			entry "ClockMask" [clock_mask $ClockMask] 4

			one_bit  7 "V8ChipBit"

			one_bit  8 "SoundHasSoundIn"
			one_bit  9 "Sound16Bit"
			one_bit 10 "SoundStereoIn"
			one_bit 11 "SoundStereoOut"
			one_bit 12 "SoundStereoMixing"
			one_bit 13 "SoundPlayAndRecord"
			one_bit 14 "SoundHasDFAC2"
			one_bit 15 "SoundLineLevel"

			one_bit 16 "SupportsIdle"
			one_bit 17 "PMgrNewIntf"

			set KeyswMask [uint32_bits 19,18]
			move -4
			if {$KeyswMask != 0} {
				entry "KeyswMask" [keysw_mask $KeyswMask] 4
			}

			one_bit 20 "MSCChipBit"
			one_bit 21 "NiagraExistsBit"
			one_bit 22 "SonoraExistsBit"
			one_bit 23 "djMEMCChipBit"

			set EgretFWMask [uint32_bits 26,25,24]
			move -4
			if {$EgretFWMask != 0} {
				entry "EgretFWMask" [egretfw_mask $EgretFWMask] 4
			}

			one_bit 27 "SupportsBtnInt"
			one_bit 28 "SupportsROMDisk"
			one_bit 29 "hasHardPowerOff"
			one_bit 30 "SoftVBL"
			one_bit 31 "hasNewMemMgr"
			move 4
		}
	} else {
		uint32 $ext
	}

	if {$ExtValid1 != 0} {
		section [format "%s1" $ext] {
			one_bit  0 "has68kEmulator"
			one_bit  1 "SerialDMA"

			set SHALMask [uint32_bits 4,3,2]
			move -4
			if {$SHALMask != 0} {
				entry "SHALMask" [shal_mask $SHALMask] 4
			}

			one_bit  5 "hasEnhancedLTalk"

			set Reserved [uint32]
			set Reserved [expr $Reserved >> 6]
			move -4
			if {$Reserved != 0} {
				set Reserved [format %X $Reserved]
#	return "$major.$minor ($$hex_version/$version)"
				entry "Reserved (bits 31-6)" "0x$Reserved" 4
			}
			move 4
		}
	} else {
		uint32 [format "%s1" $ext]
	}

	if {$ExtValid2 != 0} {
		uint32 -hex [format "%s2" $ext]
	} else {
		uint32 [format "%s2" $ext]
	}
}

proc getbits {num} {
	set res ""
	for {set i 0} {$i < 32} {incr i} {
		set res $res[expr {$num%2}]
		set num [expr {$num/2}]
	}
	return $res
}

proc parse_decoder_info {decodertableptr} {
	set returnpos [pos]
	# the DecoderInfo is at DecoderTable(FirstBaseAddr) - 40
	goto [expr $decodertableptr - 40]

	set DefaultBases [uint32]
	set DefaultBases1 [uint32]
	set DefaultBases2 [uint32]
	move -12
	set thebits [format "%s%s%s" [getbits $DefaultBases] [getbits $DefaultBases1] [getbits $DefaultBases2]]
	regsub -- "0*$" $thebits "" thebits

	parse_flags "DefaultBases" "DefExtFeatures"

	uint8 -hex "AvoidVIA1A"
	uint8 -hex "AvoidVIA1B"
	uint8 -hex "AvoidVIA2A"
	uint8 -hex "AvoidVIA2B"

	offset32code "CheckForProc" $decodertableptr

	set AddrMap [uint8]
	move -1
	set decoder_name [decoder_kind $AddrMap]
	entry "AddrMap" $decoder_name 1
	sectionname [format "DecoderInfoPtr -> DecoderInfo (%s)" $decoder_name]
	move 1

	set DecoderInfoVers [uint8]
	move -1
	entry "DecoderInfoVers" [decoderinfo_vers $DecoderInfoVers] 1
	move 1

	uint16 "filler"

	uint32 -hex "DecoderAddr"

	if {($DefaultBases | $DefaultBases1 | $DefaultBases2) != 0} {
		section "DecoderTable" {
			if {$DefaultBases != 0} {
				one_addr $thebits  0 "ROMAddr"
				one_addr $thebits  1 "DiagROMAddr"
				one_addr $thebits  2 "VIA1Addr"
				one_addr $thebits  3 "SCCRdAddr"
				one_addr $thebits  4 "SCCWrAddr"
				one_addr $thebits  5 "IWMAddr"
				one_addr $thebits  6 "PWMAddr"
				one_addr $thebits  7 "SoundAddr"
				one_addr $thebits  8 "SCSIAddr"
				one_addr $thebits  9 "SCSIDackAddr"
				one_addr $thebits 10 "SCSIHskAddr"
				one_addr $thebits 11 "VIA2Addr"
				one_addr $thebits 12 "ASCAddr"
				one_addr $thebits 13 "RBVAddr"
				one_addr $thebits 14 "VDACAddr"
				one_addr $thebits 15 "SCSIDMAAddr"
				one_addr $thebits 16 "SWIMIOPAddr"
				one_addr $thebits 17 "SCCIOPAddr"
				one_addr $thebits 18 "OSSAddr"
				one_addr $thebits 19 "FMCAddr"
				one_addr $thebits 20 "RPUAddr"
				one_addr $thebits 21 "OrwellAddr"
				one_addr $thebits 22 "JAWSAddr"
				one_addr $thebits 23 "SonicAddr"
				one_addr $thebits 24 "SCSI96Addr1"
				one_addr $thebits 25 "SCSI96Addr2"
				one_addr $thebits 26 "DAFBAddr"
				one_addr $thebits 27 "PSCAddr"
				one_addr $thebits 28 "ROMPhysAddr"
				one_addr $thebits 29 "PatchROMAddr"
				one_addr $thebits 30 "NewAgeAddr"
				one_addr $thebits 31 "Unused31Addr"
			}

			if {$DefaultBases1 != 0} {
				one_addr $thebits 32 "SingerAddr"
				one_addr $thebits 33 "DSPAddr"
				one_addr $thebits 34 "MACEAddr"
				one_addr $thebits 35 "MUNIAddr"
				one_addr $thebits 36 "AMICAddr"
				one_addr $thebits 37 "PrattAddr"
				one_addr $thebits 38 "SWIM3Addr"
				one_addr $thebits 39 "AwacsAddr"
				one_addr $thebits 40 "CivicAddr"
				one_addr $thebits 41 "SebastianAddr"
				one_addr $thebits 42 "BARTAddr"
				one_addr $thebits 43 "GrandCentralAddr"
				one_addr $thebits 44 "PBX1Addr"
				one_addr $thebits 45 "PBX2Addr"
				one_addr $thebits 46 "PBX3Addr"
				one_addr $thebits 47 "ATAAddr"
				one_addr $thebits 48 "HammerHeadAddr"
				one_addr $thebits 49 "PlatinumAddr"
				one_addr $thebits 50 "Pratt2Addr"
				one_addr $thebits 51 "PSXAddr"
				one_addr $thebits 52 "OHareAddr"
				one_addr $thebits 53 "GrackleAddr"
				one_addr $thebits 54 "HydraAddr"
				one_addr $thebits 55 "SuperIOAddr"
				one_addr $thebits 56 "SIOAddr"
				one_addr $thebits 57 "HeathrowAddr"
				one_addr $thebits 58 "MFMFloppyAddr"
				one_addr $thebits 59 "MFMMethodsVectorAddr"
				one_addr $thebits 60 "FatManAddr"
				one_addr $thebits 61 "OpenPICAddr"
				one_addr $thebits 62 "CHRPNessAddr"
				one_addr $thebits 63 "GatwickAddr"
			}

			if {$DefaultBases2 != 0} {
				one_addr $thebits 64 "Unused64Addr"
				one_addr $thebits 65 "BarAddr"
				one_addr $thebits 66 "Unused66Addr"
				one_addr $thebits 67 "Unused67Addr"
				one_addr $thebits 68 "Unused68Addr"
				one_addr $thebits 69 "Unused69Addr"
				one_addr $thebits 70 "Unused70Addr"
				one_addr $thebits 71 "Unused71Addr"
				one_addr $thebits 72 "Unused72Addr"
				one_addr $thebits 73 "Unused73Addr"
				one_addr $thebits 74 "Unused74Addr"
				one_addr $thebits 75 "Unused75Addr"
				one_addr $thebits 76 "Unused76Addr"
				one_addr $thebits 77 "Unused77Addr"
				one_addr $thebits 78 "Unused78Addr"
				one_addr $thebits 79 "Unused79Addr"
				one_addr $thebits 80 "Unused80Addr"
				one_addr $thebits 81 "Unused81Addr"
				one_addr $thebits 82 "Unused82Addr"
				one_addr $thebits 83 "Unused83Addr"
				one_addr $thebits 84 "Unused84Addr"
				one_addr $thebits 85 "Unused85Addr"
				one_addr $thebits 86 "Unused86Addr"
				one_addr $thebits 87 "Unused87Addr"
				one_addr $thebits 88 "Unused88Addr"
				one_addr $thebits 89 "Unused89Addr"
				one_addr $thebits 90 "Unused90Addr"
				one_addr $thebits 91 "Unused91Addr"
				one_addr $thebits 92 "Unused92Addr"
				one_addr $thebits 93 "Unused93Addr"
				one_addr $thebits 94 "Unused94Addr"
				one_addr $thebits 95 "Unused95Addr"
			}
		}
	}

	endsection
	goto $returnpos
	return [expr $AddrMap == 0]
}

proc parse_ram_info {infoptr} {
	set returnpos [pos]
	goto $infoptr

	set NextBankStart 0
	while {1} {
		uint32 -hex "MinBankSize"
		uint32 -hex "HighBankStart"
		uint32 -hex "HighBankEnd"
		while {1} {
			set NextBankStart [uint32]
			if {$NextBankStart == 0xFFFFFFFF} {
				move -4
				entry "NextBankStart" "end of table(s) (0xFFFFFFFF)" 4
				move 4
				break
			} elseif {$NextBankStart == 0x53616D42} {
				move -4
				entry "NextBankStart" "end of first table (SamB)" 4
				move 4
				break
			} else {
				move -4
				uint32 -hex "NextBankStart"
			}
			uint32 -hex "NextBankEnd"
		}
		if {$NextBankStart == 0xFFFFFFFF} {
			break
		}
	}

	endsection
	goto $returnpos
}

proc parse_video_info {infoptr} {
	set returnpos [pos]
	goto $infoptr

	if {1} {
		sectionname  "VideoInfoPtr -> VideoInfo (VIBuiltIn)"
		uint32 -hex "VRAMPhysAddr"
		uint32 -hex "VRAMLogAddr32"
		uint32 -hex "VRAMLogAddr24"
		uint8  -hex "SlotNumberAlias"
		uint8  -hex "SlotPramAddr"
		uint8  -hex "SuperSRsrcDirID"
		uint8  -hex "BoardSRsrcID"
		uint16 -hex "DrvrHwID"
	} else {
		sectionname  "VideoInfoPtr -> VideoInfo (VIClassic)"
		uint32 -hex "VRAMAddr"
		uint32 "ScreenByteSize"
		uint16 "ScreenTop"
		uint16 "ScreenLeft"
		uint16 "ScreenBottom"
		uint16 "ScreenRight"
		uint16 "RowByteSize"
		uint16 "RetraceRate"
		uint16 "HorizDPI"
		uint16 "VertDPI"
	}

	endsection
	goto $returnpos
}

proc parse_nubus_info {infoptr} {
	set returnpos [pos]
	goto $infoptr

	for {set i 0} {$i < 16} {incr i} {
		set slotbits [uint8]
		if {$slotbits != 0} {
			set slotstring ""
			if {$slotbits &    1} { set slotstring "$slotstring, hasPRAM" }
			if {$slotbits &    2} { set slotstring "$slotstring, canInterrupt" }
			if {$slotbits &    4} { set slotstring "$slotstring, hasConnector" }
			if {$slotbits &    8} { set slotstring "$slotstring, slotDisabled" }
			if {$slotbits & 0x10} { set slotstring "$slotstring, directSlot" }
			if {$slotbits & 0x20} { set slotstring "$slotstring, slotReserved" }
			if {$slotbits & 0x40} { set slotstring "$slotstring, dockingSlot" }
			if {$slotbits & 0x80} { set slotstring "$slotstring, bit7?" }
			move -1
			entry [format "Slot%X" $i] [string range $slotstring 2 end] 1
			move 1
		}
	}

	endsection
	goto $returnpos
}


proc parse_VIA1Init {infoptr} {
	set returnpos [pos]
	goto $infoptr

	uint8 -hex "vBufA initial value"
	uint8 -hex "vDIRA initial value"
	uint8 -hex "vBufB initial value"
	uint8 -hex "vDIRB initial value"
	uint8 -hex "vPCR initial value"
	uint8 -hex "vACR initial value"

	endsection
	goto $returnpos
}

proc parse_VIA2Init {infoptr} {
	set returnpos [pos]
	goto $infoptr

	uint8 -hex "vBufA initial value"
	uint8 -hex "vDIRA initial value"
	uint8 -hex "vBufB initial value"
	uint8 -hex "vDIRB initial value"
	uint8 -hex "vPCR initial value"
	uint8 -hex "vACR initial value"

	endsection
	goto $returnpos
}

proc parse_SndControl {infoptr} {
	set returnpos [pos]
	goto [expr $infoptr - 4]

	section [field_name_and_symbol "SndControl header" [pos]]
		uint16 -hex "flags"
		set SndTblLength [int16 "SndTblLength"]
		if {$SndTblLength == 0} {set SndTblLength 1 }
	endsection
	section [field_name_and_symbol "SndBeginTable" [pos]] {
		for {set i 0} {$i < $SndTblLength} {incr i} {
			switch $i {
				 0 { set name "sndDFACInit" }
				 1 { set name "sndDFACSend" }
				 2 { set name "sndPlaybackVol" }
				 3 { set name "sndEnableInt" }
				 4 { set name "sndDisableInt" }
				 5 { set name "sndClearInt" }
				 6 { set name "sndInputSelect" }
				 7 { set name "sndInputSource" }
				 8 { set name "sndAuxByPass" }
				 9 { set name "sndPlayThruVol" }
				10 { set name "sndAGCcontrol" }
				11 { set name "sndInitSoundHW" }
				12 { set name "dontUse1" }
				13 { set name "dontUse2" }
				14 { set name "dontUse3" }
				15 { set name "sndInitSoundHW2" }
				16 { set name "sndInitGlobals" }
				17 { set name "sndModemSound" }
				18 { set name "sndModemSndVol" }
				19 { set name "sndGetSmplRate" }
				20 { set name "sndSetSmplRate" }
				21 { set name "sndGetInputGain" }
				22 { set name "sndSetInputGain" }
				23 { set name "sndPlayThruCntl" }
				24 { set name "sndSoundHWCntl" }
				25 { set name "sndSoundHWState" }
				26 { set name "sndVirtualHWHook" }
				default { set name "Unknown ($i)" }
			}
			set codestart [offset32code $name $infoptr]
			if {$codestart & 1} {
				break
			}
		}
	}

	endsection
	goto $returnpos
}

proc parse_ClockPRAM {infoptr} {
	set returnpos [pos]
	goto [expr $infoptr - 4]

	section [field_name_and_symbol "ClockPRAM header" [pos]]
		uint16 -hex "flags"
		set tablelength [int16 "ClockPRAM table length"]
	endsection

	section [field_name_and_symbol "ClockPRAM table" [pos]] {
		for {set i 0} {$i < $tablelength} {incr i} {
			switch $i {
				0 { set name "cpInitHardware" }
				1 { set name "cpWrProtOff" }
				2 { set name "cpWrProtOn" }
				3 { set name "cpRdXByte" }
				4 { set name "cpWrXByte" }
				5 { set name "cpXPRAMIO" }
				6 { set name "cpXParam" }
				7 { set name "cpReadTime" }
				8 { set name "cpWriteTime" }
				default { set name "Unknown ($i)" }
			}
			set codestart [offset32code $name $infoptr]
			if {$codestart & 1} {
				break
			}
		}
	}

	endsection
	goto $returnpos
}

proc parse_ADBDebugUtil {infoptr} {
	set returnpos [pos]
	goto [expr $infoptr - 2]

	section [field_name_and_symbol "ADBDebugUtil header" [pos]]
		set tablelength [int16 "ADBDebugUtil table length"]
	endsection
	if {$tablelength < 20} {
		section [field_name_and_symbol "ADBDebugUtil table" [pos]] {
			for {set i 0} {$i < $tablelength} {incr i} {
				switch $i {
					0 { set name "adbInitProc" }
					1 { set name "adbEnableKbdNMI" }
					2 { set name "adbDebugEnter" }
					3 { set name "adbDebugExit" }
					4 { set name "adbDebugPoll" }
					5 { set name "adbKeySwSecure" }
					default { set name "Unknown ($i)" }
				}
				set codestart [offset32code $name $infoptr]
				if {$codestart & 1} {
					break
				}
			}
		}
	}

	endsection
	goto $returnpos
}

proc pmgr_flags {input} {
	switch $input {
		 0 { set result "PrimsTypeTable" }
		 1 { set result "PrimsTypePtr" }
		 2 { set result "PrimsTypeInfo" }
		 3 { set result "PrimsTypePMgrEx" }
		 4 { set result "PrimsTypeExp1" }
		 5 { set result "PrimsTypeExp2" }
		 6 { set result "PrimsTypeExp3" }
		 7 { set result "PrimsTypeExp4" }
		default { set result "Unknown ($input)" }
	}
	return $result
}

proc parse_PowerManager {infoptr} {
	set returnpos [pos]
	goto [expr $infoptr - 8]

	section [field_name_and_symbol "PmgrPrimitivesRec" [pos]]
		set PmgrPrimsFlags [uint32]
		move -4
		entry "PmgrPrimsFlags" [pmgr_flags $PmgrPrimsFlags] 4
		move 4
		set tablelength [expr [int32 "PmgrPrimsCount"] / 4]
	endsection
	section [field_name_and_symbol "PowerManager table" [pos]] {
		for {set i 0} {$i < $tablelength} {incr i} {
			switch $i {
				 0 { set name "PmgrRoutineTbl" }
				 1 { set name "PrimInfoTblPtr" }
				 2 { set name "IdleMindTblPtr" }
				 3 { set name "SleepTblPtr" }
				 4 { set name "WakeTblPtr" }
				 5 { set name "PMgrOpExcepTbl" }
				 6 { set name "ModemTblPtr" }
				 7 { set name "PwrDispatchTbl" }
				 8 { set name "PmgrHookTbl" }
				 9 { set name "PmgrCommTblPtr" }
				10 { set name "PMgrOpTblPtr" }
				11 { set name "BklightTblPtr" }
				default { set name "Unknown ($i)" }
			}
			set codestart [offset32code $name $infoptr]
			if {$codestart & 1} {
				break
			}
		}
	}

	endsection
	goto $returnpos
}

proc vector_address_name {input} {
	if { 0 } {
	} elseif { $input ==   0x64 } { set result "AutoInt1 (6)"
	} elseif { $input ==  0x192 } { set result "Lvl1DT (8) and Lvl2DT (8)"
	} elseif { $input ==  0x1B2 } { set result "Lvl2DT (8)"
	} elseif { $input ==  0x2BE } { set result "ExtStsDT (4)"
	} elseif { $input == 0x0D70 } { set result "VIA2DT (8)"
	} else {
		set result [format "Unknown (0x%X)" $input]
	}
	return $result
}

proc parse_vector_table {infoptr} {
	set returnpos [pos]
	goto $infoptr

	set vector_address [uint32]
	move -4
	entry "vector address" [vector_address_name $vector_address] 4
	move 4
	set vector_handler_number 1
	while {1} {
		set codestart [offset32code "vector handler $vector_handler_number" [expr [pos] + 4]]
		if {$codestart <= 0 || $codestart >= [len]} {
			break
		}
		set vector_handler_number [expr $vector_handler_number + 1]
		if {$vector_handler_number > 64} {
			break
		}
	}

	endsection
	goto $returnpos
}

proc parse_IntHandler {infoptr} {
	set returnpos [pos]
	for {set numprims 10} {$numprims <= 11} {incr numprims} {
		goto [expr $infoptr - 4]
		section -collapsed [field_name_and_symbol [format "InterruptPrims (%d)" $numprims] [pos]] {
			section [field_name_and_symbol "IntHandler header" [pos]]
				uint16 -hex "flags"
				set tablelength [int16 "count"]
			endsection

			if {$tablelength < 30} {
				section [field_name_and_symbol "IntHandler table" [pos]] {
					for {set i 0} {$i < $tablelength} {incr i} {
						if {$i < $numprims} {
							switch $i {
								 0 { set name "intInitPostProc" }
								 1 { set name "intDisableInts" }
								 2 { set name "intEnableOneSec" }
								 3 { set name "intEnableSlots" }
								 4 { set name "intEnableSound" }
								 5 { set name "intDisableSound" }
								 6 { set name "intClearSound" }
								 7 { set name "intEnableSCSI" }
								 8 { set name "intDisableSCSI" }
								 9 {
									if {$numprims == 11} {
										set name "intClearSCSIInt"
									} else {
										# I believe this is correct for numprims == 10.
										# I don't know if any of the above are also incorrect for numprims == 10.
										set name "intPowerOffProc"
									}
								 }
								10 { set name "intPowerOffProc" }
							}
							set codestart [offset32code $name $infoptr]
							if {$codestart & 1} {
								break
							}
						} else {
							set name [format "VectTbl (%d)" [expr $i - $numprims]]
							set codestart [offset32section $name [expr [pos] + 4]]
							if {$codestart != 0} {
								parse_vector_table $codestart
							}
						}
					}
				}
			}
		}
	}

	endsection
	goto $returnpos
}

proc parse_ImmgPrim {infoptr} {
	set returnpos [pos]
	goto $infoptr

	bytes 2 "ImmgPrim Data"

	endsection
	goto $returnpos
}

proc parse_IconInfo {infoptr} {
	set returnpos [pos]
	goto $infoptr

	bytes 2 "IconInfo Data"

	endsection
	goto $returnpos
}

set first_productinfo 0x7FFFFFFF

proc parse_product_info {infoptr} {
	set returnpos [pos]
	goto $infoptr

	global first_productinfo
	if {$infoptr < $first_productinfo} {
		set first_productinfo $infoptr
	}

	set DecoderInfoPtr [offset32section "DecoderInfoPtr -> FirstBaseAddr" $infoptr]
	if {$DecoderInfoPtr != 0} {
		parse_decoder_info $DecoderInfoPtr
	}

	set RamInfoPtr [offset32section "RamInfoPtr -> RamBankInfo" $infoptr]
	if {$RamInfoPtr != 0} {
		parse_ram_info $RamInfoPtr
	}

	set VideoInfoPtr [offset32section "VideoInfoPtr -> VideoInfo" $infoptr]
	if {$VideoInfoPtr != 0} {
		parse_video_info $VideoInfoPtr
	}

	set NuBusInfoPtr [offset32section "NuBusInfoPtr -> NuBusInfo" $infoptr]
	if {$NuBusInfoPtr != 0} {
		parse_nubus_info $NuBusInfoPtr
	}

	set HwCfgWord [uint16]
	move -2
	if {$HwCfgWord != 0} {
		section "HwCfgWord" {
			one_bit16 15 "hwCbSCSI"
			one_bit16 14 "hwCbClock"
			one_bit16 13 "hwCbExPRAM"
			one_bit16 12 "hwCbFPU"
			one_bit16 11 "hwCbMMU"
			one_bit16 10 "hwCbADB"
			one_bit16 9  "hwCbAUX"
			one_bit16 8  "hwCbPwrMgr"
			set reserved [uint16_bits 7,6,5,4,3,2,1,0]
			if {$reserved != 0} {
				move -2
				uint16_bits 7,6,5,4,3,2,1,0 "(reserved)"
			}
		}
	} else {
		uint16 "HwCfgWord"
	}

	set ProductKind [uint8]
	move -1
	entry "ProductKind" [product_kind $ProductKind] 1
	move 1

	set DecoderKind [uint8]
	move -1
	entry "DecoderKind" [decoder_kind $DecoderKind] 1
	move 1

	set Rom85Word [uint16]
	move -2
	entry "Rom85Word" [rom85_word $Rom85Word] 2
	move 2

	set DefaultRSRCs [uint8]
	move -1
	entry "DefaultRSRCs" [default_rsrcs $DefaultRSRCs] 1
	move 1

	set ProductInfoVers [uint8]
	move -1
	entry "ProductInfoVers" [productinfo_vers $ProductInfoVers] 1
	move 1

	parse_flags "BasesValid" "ExtValid"

	set product_from_via ""
	set VIAIdMask [uint32]
	set VIAIdMatch [uint32]
	if {$VIAIdMask != 0} {
		move -8
		if {$VIAIdMask == 0x40000008} {
			entry "VIAIdMask" "Check VIA1 PA6, VIA2 PB3" 4
			move 4
			set product_from_via [product_pa6pb3 $VIAIdMatch]
			entry "VIAIdMatch" $product_from_via 4
			move 4
		} elseif {$VIAIdMask == 0x56000000} {
			entry "VIAIdMask" "Check VIA1 PA6, PA4, PA2, PA1" 4
			move 4
			set product_from_via [product_via_pa6421 $VIAIdMatch]
			entry "VIAIdMatch" $product_from_via 4
			move 4
		} else {
			entry "VIAIdMask" [format "Unknown (0x%08X)" $VIAIdMask] 4
			move 4
			entry "VIAIdMatch" [format "Unknown (0x%08X)" $VIAIdMatch] 4
			move 4
		}
	} else {
		move -4
		uint32 "(YMCA|MMC)IdMatch"
	}

	set VIA1InitPtr [offset32section "VIA1InitPtr -> VIA1Init" $infoptr]
	if {$VIA1InitPtr != 0} {
		parse_VIA1Init $VIA1InitPtr
	}
	set VIA2InitPtr [offset32section "VIA2InitPtr -> VIA2Init" $infoptr]
	if {$VIA2InitPtr != 0} {
		parse_VIA2Init $VIA2InitPtr
	}
	set SndControlPtr [offset32section "SndControlPtr -> SndBeginTable" $infoptr]
	if {$SndControlPtr != 0} {
		parse_SndControl $SndControlPtr
	}
	set ClockPRAMPtr [offset32section "ClockPRAMPtr -> ClockPRAM table" $infoptr]
	if {$ClockPRAMPtr != 0} {
		parse_ClockPRAM $ClockPRAMPtr
	}
	set ADBDebugUtilPtr [offset32section "ADBDebugUtilPtr -> ADBDebugUtil table" $infoptr]
	if {$ADBDebugUtilPtr != 0} {
		parse_ADBDebugUtil $ADBDebugUtilPtr
	}
	set PowerManagerPtr [offset32section "PowerManagerPtr -> PowerManager table" $infoptr]
	if {$PowerManagerPtr != 0} {
		parse_PowerManager $PowerManagerPtr
	}
	set IntHandlerPtr [offset32section "IntHandlerPtr -> IntHandler table" $infoptr]
	if {$IntHandlerPtr != 0} {
		parse_IntHandler $IntHandlerPtr
	}
	set ImmgPrimPtr [offset32section "ImmgPrimPtr" $infoptr]
	if {$ImmgPrimPtr != 0} {
		parse_ImmgPrim $ImmgPrimPtr
	}

	set product_from_cpuid ""
	set CPUIDValue [uint16]
	if {$CPUIDValue != 0} {
		move -2
		set product_from_cpuid [product_cpuid $CPUIDValue]
		entry "CPUIDValue" $product_from_cpuid 2
		move 2
	}

	set product [productinfo_name $ProductKind $DecoderKind $VIAIdMask $VIAIdMatch $CPUIDValue]
	if {$product == ""} {
		if {$ProductKind != 253} {
			set product [product_kind $ProductKind]
		}
		if {$product_from_via != ""} {
			if {$product != ""} {
				set product "$product ; "
			}
			set product "$product$product_from_via"
		}
		if {$product_from_cpuid != ""} {
			if {$product != ""} {
				set product "$product ; "
			}
			set product "$product$product_from_cpuid"
		}
	}
	if {$product != ""} {
		sectionname $product
	}

	uint16 "padding"

	set IconInfoPtr [offset32section "IconInfoPtr" $infoptr]
	if {$IconInfoPtr != 0} {
		parse_IconInfo $IconInfoPtr
	}

	endsection
	goto $returnpos
}

proc parse_univ_tables {univtables} {
	if {$univtables > 0 && $univtables < [len]} {
		goto $univtables
		section -collapsed "Universal Tables" {
			sectionvalue [offsetname $univtables]
			section "CPUIDProductLookup" {
				set addr 1
				while {$addr != 0} {
					set addr [offset32section "ProductInfo" [pos]]
					if {$addr != 0} {
						parse_product_info $addr
					}
				}
			}
			section "ProductLookup" {
				set addr 1
				while {$addr != 0} {
					set addr [offset32section "ProductInfo" [pos]]
					if {$addr != 0} {
						parse_product_info $addr
					}
				}
			}
			section "DecoderLookup" {
				set done 0
				while {!$done} {
					set addr [offset32section "DecoderInfo" [pos]]
					if {$addr != 0} {
						set done [parse_decoder_info $addr]
					} else {
						set done true
					}
				}
			}
			global first_productinfo
			goto $first_productinfo
			entry "First ProductInfo" "" 1
		}
	}
}

#### Main parser

## Stage 1: DeclROM

set dir_start -1

if {$magic == 0x5A932BC7} {
	# Jump to the end where the header is
	goto $end_of_rom
	section -collapsed "DeclROM"

	# Step backwards through the header
	move -1
	set raw_lanes [uint8]
	move -1
	entry "ByteLanes" [byte_lanes $raw_lanes] 1
	move 1
	move -6
	hex 4 "TestPattern"
	move -5
	uint8 "Format"
	move -2
	uint8 "RevisionLevel"
	move -5
	hex 4 "CRC"
	move -8
	set length [uint32 "Length"]

	move -7
	set dir_start [offset24section "Directory" [expr $end_of_rom - 20]]
	parse_rsrc_dir $dir_start
	endsection
	endsection
}

## Stage 2: Extended DeclROM

goto [expr $end_of_rom-24]
set extended_magic [uint32]
if {$extended_magic == 0x5A932BC7} {
	section -collapsed "Extended DeclROM"
	move -4
	hex 4 "TestPattern"
	move -16
	section -collapsed "Extended Directory"

	section -collapsed "SuperInit"
	section -collapsed "Metadata"
	set rsrc_type [uint8 "Type"]
	set offset [int24 "Offset"]
	set next_directory_item_pos [pos]
	endsection
	sectionname "SuperInit ($rsrc_type)"
	exec_block [expr $offset-4]
	endsection

	goto $next_directory_item_pos
	section -collapsed "SuperDirectory"
	section -collapsed "Metadata"
	set rsrc_type [uint8 "Type"]
	set offset [int24 "Offset"]
	set next_directory_item_pos [pos]
	endsection
	sectionname "Super Directory ($rsrc_type)"
	goto [expr $end_of_rom - 32 + $offset]
	# This is an sResource of pointers to sResource directories for each board
	# Loop over the top level sResource entries
	set rsrc_offset 1
	set rsrc_type 0
	while {[expr $rsrc_offset != 0x000000 && $rsrc_type != 0xFF]} {
		section "sRsrcDir"

		section -collapsed "Metadata"
		set rsrc_type [uint8 "Type"]
		set rsrc_offset [int24 "Offset"]
		endsection
		if {$rsrc_type == 0xFF} {
			sectionname "Terminator (255)"
		} else {
			sectionname "Directory ($rsrc_type)"
			# TODO: This is dumb
			set oldpos [pos]
			move -4
			move $rsrc_offset
			parse_rsrc_dir [pos]
			goto $oldpos
		}
		endsection
	}
	endsection

	while (1) {
		goto $next_directory_item_pos
		section "Directory"
		section -collapsed "Metadata"
		set rsrc_type [uint8 "Type"]
		set rsrc_offset [int24 "Offset"]
		set next_directory_item_pos [pos]
		endsection
		if {$rsrc_type == 0xFF} {
			sectionname "Terminator (255)"
		} else {
			sectionname "Directory ($rsrc_type)"
		}
		endsection
		if {$rsrc_type == 0xFF} {
			break
		}
	}

	endsection
	endsection
}

## Stage 3: System ROM

# TODO: This isn't quite right with Extended Format
if {$dir_start != 0} {
	# TODO: From here on we match a System ROM using the value of the reset vector. This is quite odd and probably imperfect.

	set machine ""

	# If we didn't find a DeclROM, then it has to be a System ROM, or it's not a supported file type
	if {$dir_start == -1} {
		goto 0
		set chrp_boot [ascii 11]
		if {$chrp_boot == "<CHRP-BOOT>"} {
			section "New World ROM"
			entry "TODO" "TODO" 1
			endsection

			# For now we early return
			return
		}
		# TODO: Requirements have been temporarily disabled to support Twiggy ROMs
		#requires 6 "00 2a"
	}
	# Search to see if this is a system board ROM
	# https://mcosre.sourceforge.net/docs/rom_v.html
	goto 6
	# TODO: We're detecting the ROM by checking the reset vector, which is odd, but it's always 0x2A
	set data [uint16]
	if {$data == 0x2A || $data == 0x16} {
		goto 0
		section -collapsed "System ROM"

		# TODO: This is a guess, we know the early Twiggy ROMs with a different reset vector don't have a checksum
		if {$data == 0x2A} {
			set checksum [uint32 -hex "Checksum"]
			set hex_checksum [format %08X $checksum]
		} else {
			set hex_checksum "none"
			move 4
		}

		set univtables 0
		set filename "rom_maps/$hex_checksum"
		if { [file exists $filename] == 1 } {
			goto 0
			section -collapsed "Symbols"
			set map [open $filename "r"]
			set lines [split [read $map] "\n"]
			close $map
			foreach line $lines {
				# Skip empty lines
				if {$line == ""} {
					continue
				}
				set data [split $line " "]
				set str_offset [lindex $data 1]
				regsub -- "^$" $str_offset "0x" str_offset

				scan $str_offset %x raw_offset
				goto $raw_offset
				entry [lindex $data 0] [format "0x%X" $raw_offset] 1
				dict set symbolsdict $raw_offset [lindex $data 0]
				if {[lindex $data 0] == "UNIVTABLES"} {
					set univtables $raw_offset
				}
			}
			endsection
		}

		goto 8
		section "Versions"
		# TODO: Also format in the $XXXX format used in some places
		set machine [uint8 "Machine"]
		move -1
		set family_version [uint16]
		move -2
		entry "Family Version" [format $%04X $family_version] 2
		move 2
		move -1
		# TODO: Classify by type
		set minor_ver [uint8]
		move -1
		entry "ROM Version" [rom_version $minor_ver] 1
		if {[universal_rom $machine]} {
			goto 18
			set rom_release [uint16]
			move -2
			entry "Minor Version" [rom_release $rom_release] 2
			goto 76
			uint16 "Sub Release"
		}

		# Read the date from old-style ROMs
		# No DeclROM and versions between 7.5 and 7.11
		if {$dir_start == -1 && $minor_ver >= 0x75 && $minor_ver < 0x7B} {
			# Always look at the 256k or 512k offsets in case a ROM disk was appended
			if {$minor_ver == 0x75} {
				goto [expr 0x20000 - 1]
			} else {
				goto [expr 0x40000 - 1]
			}
			set date_length [uint8]
			move [expr -$date_length - 1]
			ascii $date_length "Build Date"
		} elseif {$rom_date != -1} {
			# Borrow the build date from the DeclROM if we found one
			goto $rom_date
			cstr "macroman" "Build Date (DeclROM)"
		}
		endsection

		goto 0
		section -collapsed "ROM Header"
		sectionvalue [offsetname 0]

		goto 4
		set ResetVector [offset32section "Reset Vector" 0]
		if {$ResetVector != 0} {
			set thispos [pos]
			goto $ResetVector
			jmp "Reset Entry"
			goto $thispos
			endsection
		}

		goto 10
		# TODO: Determine how to read pre-Universal ROM headers
		if {
			[universal_rom $machine] || (
				[dict exists $symbolsdict 34] &&
				[dict get $symbolsdict 34] == "DISPOFF"
			)
		} {
			jmp "Start Boot Vector"
			jmp "Bad Disk Vector"
			move 2
			uint8 "Patch Flags"
			move 1
			set ForeignOS [offset32section "Foreign OS Vector Table" 0]
			if {$ForeignOS != 0} {
				goto $ForeignOS
				offset32code "Initializes A-trap dispatch tables" 0
				offset32code "A-trap dispatcher" 0
				offset32code "Handler for unimplemented traps" 0
				if {[universal_rom $machine]} {
					offset32code "Initializes the Slot Manager" 0
					offset32code "Initializes the Memory Manager jump tables" 0
					offset32code "MMU switch code" 0
					set InitRomVectors [offset32section "InitRomVectors" 0]
					if {$InitRomVectors != 0} {
						goto $InitRomVectors
						set vector_num 0
						while {1} {
							set instruction [uint16]
							if {$instruction != 0x61FF} {
								break
							}
							move -2
							jmp [format "Vector %d" $vector_num]
							set vector_num [expr $vector_num + 1]
						}
						endsection
					}
				}
				endsection
			}
		} else {
			for {set i 0} {$i < 4} {incr i} {
				if {[int16] == 0x6000} {
					move -2
					jmp "Unknown BRA"
				} else {
					move 2
				}
			}
		}

		# Both ROM eras support resource data offset
		if {[universal_rom $machine] || [legacy_resources $minor_ver]} {
			goto 0x1a
			set resource_data_offset [offset32 "Resource Data Offset" 0]
		}

		goto 0x1e
		if {
			[universal_rom $machine] || (
				[dict exists $symbolsdict 34] &&
				[dict get $symbolsdict 34] == "DISPOFF"
			)
		} {
			jmp "Eject Vector"
		} elseif {[int16] == 0x4EFA} {
			move -2
			jmp "Unknown JMP"
		}

		if {
			[universal_rom $machine] || (
				[dict exists $symbolsdict 34] &&
				[dict get $symbolsdict 34] == "DISPOFF"
			)
		} {
			goto 0x22
			set DispTable [offset32section "Dispatch Table" 0]
			if {$DispTable != 0} {
				goto $DispTable

				sectioncollapse
				section -collapsed "ToolBox" {
					for {set i 0} {$i <= 0x3FF} {incr i} {
						set addr [offset32code [format "ToolBox $%03X" $i] 0]
						if {$addr < 0 || $addr >= [len]} {
							sectionname "ToolBox (invalid)"
							break
						}
					}
				}

				set osnum ""
				for {set os 0} {$os <= 1} {incr os} {
					goto [expr $DispTable + (0x400 + $os * 0x100) * 4]
					section -collapsed "OS$osnum" {
						for {set i 0} {$i <= 0xAE} {incr i} {
							set addr [offset32code [format "OS%s $%02X" $osnum $i] 0]
							if {$addr < 0 || $addr >= [len]} {
								sectionname "OS$osnum (invalid)"
								break
							}
						}
					}

					goto [expr $DispTable + (0x400 + $os * 0x100 + 0xAF) * 4]
					section -collapsed "Vectors$osnum" {
						for {set i 0xAF} {$i <= 0xFF} {incr i} {
							set addr [offset32code [format "OS%s $%02X" $osnum $i] 0]
							if {$addr < 0 || $addr >= [len]} {
								sectionname "Vectors$osnum (invalid)"
								break
							}
						}
					}
					set osnum "2"
				}

				endsection
			}
		}

		if {
			[universal_rom $machine] || (
				[dict exists $symbolsdict 38] &&
				[dict get $symbolsdict 38] == "CRITICAL"
			)
		} {
			goto 0x26
			jmp "Critical Error Vector"
		}

		if {$ResetVector == 0x2A} {
			goto 0x2a
			jmp "Reset Entry"
		}

		if {[universal_rom $machine]} {
			goto 0x2e
			uint8 "ROM Location Bit"
			move 1
			uint32 -hex "Checksum (Chunk 1)"
			uint32 -hex "Checksum (Chunk 2)"
			uint32 -hex "Checksum (Chunk 3)"
			uint32 -hex "Checksum (Chunk 4)"
			set rom_size [uint32 -hex "ROM Size"]
			offset32code "Erase Happy Mac Vector" 0
			offset32code "Toolbox Init Vector" 0
		}

		endsection

		parse_univ_tables $univtables

		if {[universal_rom $machine]} {
			goto $resource_data_offset
			section -collapsed "Resources"
			sectionvalue [offsetname $resource_data_offset]
			section -collapsed "Metadata"
			set next [offset32 "First Entry Offset" 0]
			uint8 "Max Valid Index"
			set combo_size [uint8 "Combo Mask Size"]
			uint16 "Combo Mask Version"
			set header_size [uint16 "Header Size"]
			endsection

			while {$next != 0} {
				goto $next
				section -collapsed "Resource"
				set combo_data [hex $combo_size]
				move -$combo_size
				entry "Combo Mask" [combos $combo_data] $combo_size
				move $combo_size
				set next [offset32 "Next Entry Offset" 0]
				set next_data [offset32 "Data Offset" 0]
				set type [str 4 macroman "Type"]
				set id [uint16 "ID"]
				uint8 -hex "Attributes"
				set name [pstr macroman "Name"]
				if {$name != ""} {
					sectionname "$type \[$name\] ($id)"
				} else {
					sectionname "$type ($id)"
				}
				goto $next_data
				move [expr -$header_size]
				if {$header_size == 12} {
					uint32 -hex "More Attributes?"
				}
				set data_size [uint32 "Size"]
				uint32 "Fake pointer?"
				if {[expr $data_size-$header_size] > 0} {
					bytes [expr $data_size-$header_size] "Data"
				}
				# TODO: Add resource handlers
				if {$type == "CURS"} {
					goto $next_data
					bytes 32 "Cursor Data"
					bytes 32 "Cursor Mask"
					bytes 4 "Cursor Point"
				}
				endsection
			}
			endsection
		} elseif {[legacy_resources $minor_ver]} {
			goto $resource_data_offset
			section -collapsed "Resources"
			sectionvalue [offsetname $resource_data_offset]
			section -collapsed "Metadata"
			int16 "Total Number of Resources"
			# TODO: Why 28? Inside Macintosh indicates this should be 16+4+2+2 = 24
			#  It seems probable this starts with the Single Resource data (Figure 1-13) instead of a real resource fork
			#  Thus we have the length, then a copy of the resource header (16), 6 reserved bytes, then our offsets
			#  == 28
			goto [expr $resource_data_offset + 28]
			set typelist_addr [offset16zero "Type List Offset" [expr $resource_data_offset + 4]]
			set namelist_addr [offset16zero "Name List Offset" [expr $resource_data_offset + 4]]
			set num [uint16 "Num Types"]
			endsection
			for {set i 0} {$i <= $num} {incr i} {
				section -collapsed "Resource"
				section -collapsed "Metadata"
				set type [ascii 4 "Type"]
				set num_resources [uint16 "Num Resources (0 indexed)"]
				set list_addr [offset16zero "List Offset" $typelist_addr]
				endsection
				sectionname "$type"
				set cur_pos [pos]
				goto [expr $list_addr]
				for {set j 0} {$j <= $num_resources} {incr j} {
					section -collapsed "Resource"
					set id [uint16 "ID"]
					set name_offset [uint16]
					move -2
					if {$name_offset != 0xFFFF} {
						set name_addr [offset16zero "Name Offset" $namelist_addr]
					} else {
						entry "Name Offset" "-1" 2
						move 2
					}
					sectionname "$type ($id)"
					if {$name_offset != 0xFFFF} {
						set res_pos [pos]
						goto $name_addr
						set name [pstr macroman "Name"]
						if {$name != ""} {
							sectionname "$type \[$name\] ($id)"
						}
						goto $res_pos
					}
					uint8 -hex "Attributes"
					set rsrc_offset [offset24 "Data Offset" 0]
					set res_pos [pos]
					# Length includes the attributes and offset, so subtract
					goto [expr $rsrc_offset - 6]
					set data_length [uint16 "Resource Data Length"]
					# TODO: What is this?
					move 4
					# Length include the header so subtract
					bytes [expr $data_length - 6] "Data"
					goto $res_pos
					# This is reserved, so skip
					move 4
					endsection
				}
				goto $cur_pos
				endsection
			}
			endsection
		}
		endsection
	}

	# Search for EDisks
	# New Technical Notes HW 13 - Macintosh Portable ROM Expansion
	# These can occur at any 64k boundary
	# TODO: Verify Ginty works correctly
	set edisk_offset 0
	set edisk_count 0
	while {$edisk_offset < [len]} {
		goto $edisk_offset
		move 132
		set edisk_magic [bytes 12]

		if {$edisk_magic == "EDisk Gary D"} {
			set edisk_type "edisk"
		} elseif {$edisk_magic == "Ginty HYGWGA"} {
			set edisk_type "ginty"
		} else {
			set edisk_type ""
		}

		if {$edisk_type != ""} {
			if {$edisk_type == "edisk"} {
				section -collapsed "EDisk ($edisk_count)"
			} else {
				section -collapsed "EDisk (Ginty) ($edisk_count)"
			}

			goto $edisk_offset

			section -collapsed "Metadata"
			bytes 128 "Scratch Space"
			uint16 "Block Size"
			uint16 "Version"
			hex 12 "EDisk Magic"
			uint32 "Device Size"
			# TODO: Read times correctly
			uint32 "Format Time"
			uint32 "Format Ticks"
			# TODO: Read checksum field
			uint32 "Format Checksum Offset"
			set data_start [uint32 "Data Start Offset"]
			set data_end [uint32 "Data End Offset"]
			uint32 "Media Icon Offset"
			uint32 "Drive Icon Offset"
			# TODO: Document better
			uint32 "'Get Info Where' String Offset"
			uint32 "Drive Info"
			if {$edisk_type == "edisk"} {
				bytes 328 "Reserved"
			} else {
				# TODO: Read data
				uint32 "EDisk Driver Offset"
				uint32 "Loader Patch Code Offset"
				bytes 320 "Reserved"
			}
			endsection

			# Disk images can span past the end of the image and "virtually" appear larger(!), so
			# cap the read.
			# Theoretically you could do wild tricks with this and let memory wraparound map more
			# data into the image but that's pretty unlikely.
			if {[expr $edisk_offset + $data_start + $data_end] > [len]} {
				set data_end [expr [len] - $edisk_offset]
			}

			goto $edisk_offset
			move $data_start
			bytes [expr $data_end - $data_start] "Disk Image"
			endsection
			set edisk_count [expr $edisk_count + 1]
		}
		set edisk_offset [expr $edisk_offset + 0x10000]
	}

	# TODO: Most of the time these images are just catted at the end, but technically the offset
	# can vary. We're just making a best effort.
	# TODO: We don't read the length either, so we're just reading all the way to the end
	# TODO: Compression could break this, but it's unlikely
	if {[universal_rom $machine] && [len] > $rom_size} {
		goto $rom_size
		set hfs_magic [uint16]
		if {$hfs_magic == 0x4C4B} {
			move -2
			section -collapsed "bbraun/BMOW Rom Disk"
			bytes eof "Disk Image (Approximate)"
			endsection
		}
	}
}
