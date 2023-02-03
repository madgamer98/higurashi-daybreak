#!/usr/bin/perl
use strict;
use warnings;
use Fcntl ('SEEK_SET', 'SEEK_CUR', 'SEEK_END');
use Encode qw/encode decode/;

#Script created by Roger Pepitone.
#2023: Edited by Chase Dalton Pollard / madgamer98

#Some usage notes from HigurashiArchive: 
#* The script needs to be run in binary mode, so it won't work on a Windows machine out of the box. There is a way to get it to work on Windows but it's easier and faster to throw it on a Linux machine instead because Linux opens files in binmode by default.
#* I have not really tested the functionality of the --update or --list options. In theory you could use --update to create mods, but the game is impossible to run on modern computers anyway.
#* --extract requires either two or three args (input .dat file, output destination location, and optionally a pattern). The two-arg version doesn't seem to work as intended, because it will fail to deobfuscate certain files, in particular .x models of some of the characters. Using the regex pattern [.]* for the pattern seems to make the missing items decrypt as intended, but I have no way of knowing this 100% for sure. There may actually be something missing. I checked the size of the DAT file with the output directory and they were within a few hundred kb of each other, so I felt fairly confident about its output.

#More usage notes from madgamer98
#Script works fine on my windows machine out of the box. I've made the changes needed to have it run in binary mode (binmode) anyway just in case.
#Game seems to run fine on Windows 10/11 as long as you set your CPU core affinity to 1 core.

# This subroutine decrypts a block of data in the file table
sub decrypt_file_table_block {
    # Parameters are the index of the block and the encrypted string
    my ($index, $str) = @_;
    
    # Masks the index to 16 bytes (0x1ff = 511 in decimal)
    $index = $index & 0x1ff;

    # Initialize the counter (ctr) and key for the decryption process
    # The counter is calculated as (100 + index * 77) & 0xff (masked to 8 bytes)
    # The key is calculated as (100 * (index + 1) + (255 & (index * (index - 1) / 2)) * 77) & 0xff (masked to 8 bytes)
    my $ctr = (100 + $index*77) & 0xff;
    my $key = (100*($index+1) + (0xff&($index*($index-1)/2))*77) & 0xff;

    # Initialize an empty string to store the decrypted data
    my $rv = '';
    
    # Loop through each character in the encrypted string
    for (my $i = 0; $i < length ($str); ++ $i) {
        # XOR the character with the key and add it to the decrypted data string
        $rv .= chr (ord (substr ($str, $i, 1)) ^ $key);
        # Update the key and counter for the next iteration
        $key = ($key + $ctr) & 0xff;
        $ctr = ($ctr + 77) & 0xff;
    }
    # Return the decrypted data
    return $rv;
}

# This subroutine grabs a array/hash table of the files data.
sub get_table_data {
    # Define the input file handle ($IFH) as the argument passed to the function
    my ($IFH) = @_;

    # Seek to the start of the file or die with an error message if there is a problem
    seek ($IFH, 0, SEEK_SET) or die "Error seeking to start of file: $!";

    # Declare a scalar variable $data to store the data read from the file
    my $data;
    # Read two bytes from the input file into the $data variable. Die with an error message if there is a problem
    read ($IFH, $data, 2) == 2 or die "Error reading table length: $!";
    # Unpack the two bytes read into the $data variable into a binary number and store it in the $n_files variable
    my $n_files = unpack ('v', $data);

    # Read the next 268 * $n_files bytes from the input file into the $data variable. Die with an error message if there is a problem
    read ($IFH, $data, 268 * $n_files) == 268*$n_files or die "Error reading in table: $!";

    # Call the function `decrypt_file_table_block` with the starting position (0) and the $data variable as arguments, and store the result in $data2
    my $data2 = decrypt_file_table_block (0, $data);

    # Declare an array variable @table to store the file table data
    my @table = ();
    # Declare a hash variable %table to store the file table data
    my %table = ();

    # Loop through each of the $n_files in the file table
    for (my $i = 0; $i < $n_files; ++ $i) {
        # Unpack the next 268 bytes of the $data2 variable into variables $fname, $len, and $offset
        my ($fname, $len, $offset) = unpack ('a260VV', substr ($data2, $i*268, 268));

        # Set the variable $j to 0
        my $j = 0;
        # Loop while $j is less than the length of the $fname string and the character at position $j in $fname is not a null character
        while ($j < length ($fname) && substr ($fname, $j, 1) ne "\0") {++$j;}
    
        # Set $fname to a decoded version of the first $j characters of the $fname string, using the "shift_jis" encoding
        $fname = decode ("shift_jis", substr ($fname, 0, $j));
        # Create a hash reference $tmp with keys "index", "offset", "length", and "name", and values $i, $offset, $len, and $fname, respectively
        my $tmp = { index => $i, offset => $offset, length => $len, name => $fname };
        # Push the $tmp reference onto the end of the @table array
        push @table, $tmp;
        # Set the value of the key $fname in the %table hash to the $tmp reference
        $table{$fname} = $tmp;
    }
    return (\%table, \@table);
}

# show_file_table - prints out the contents of a file table

sub show_file_table {
    # set the output encoding for standard output to UTF-8
    binmode STDOUT, ":utf8";

    # extract the file name from the input arguments
    my ($fname) = @_;

    # open the file for reading and assign it to the file handle $FH
    my $FH;
    open ($FH, '<', $fname) or die "Unable to open file: $!";
    binmode $FH;

    # call get_table_data on the file handle and extract the resulting hash and list
    my ($hash, $list) = get_table_data ($FH);

    # print the contents of the hash
    print "HASH:\n";
    foreach (sort keys %$hash) {
        # print each key and its values
        print " $_ -> ";
        foreach my $nested_key (sort keys %{$hash->{$_}}) {
            # extract the value of the nested key
            my $printedHasHValue = %{$hash->{$_}}{$nested_key};
            # print the nested key and value, but skip the key "index"
            if ($nested_key ne "index") {
                print $nested_key . " : " . $printedHasHValue . " ";
            }
        }
        print "\n";
    }

    # print the contents of the list
    print "\nLIST:\n";
    foreach my $h (@$list) {
        # create an array of key-value pairs
        my @pname = ();
        foreach my $sh (sort keys %$h) {
            # push each key and value onto the array
            push @pname, $sh;
            push @pname, $h->{$sh};
        }
        # print the array as a string, with each pair separated by a comma
        print "  ", join (", ", @pname), "\n";
    }
}

# This subroutine lists the files in a given bundle file.
sub list_bundle {
    # Accepts the path to the bundle file and the extract path as input
    my ($bundle_path, $extract_path) = @_;

    # If the bundle path does not exist, print an error message and exit
    die "$bundle_path does not exist" if (! -f $bundle_path);

    # Set the standard output stream to UTF-8 encoding
    binmode STDOUT, ":utf8";

    # Open the input file handle for the bundle file
    my $IFH;
    open $IFH, "<", $bundle_path or die "Unable to open $bundle_path";
    binmode $IFH;

    # Get the table data of the bundle file
    my ($hash, $list) = get_table_data ($IFH);
    my ($str, $str2, $k);

    # Iterate through the list of files in the bundle file
    foreach my $h (@$list) {
        # Create an array to store the names of each file in the bundle file
        my @pname = ();
        foreach my $sh (sort keys %$h) {
            # Push the file names and their details into the array
            push @pname, $sh;
            push @pname, $h->{$sh};
        }
        # Print the names and details of the files in the bundle file
        print "  ", join (", ", @pname), "\n";
    }
}

# extract_bundle - extract files from a bundle to the specified extract path.
#
# @bundle_path: path to the bundle file
# @extract_path: path to extract the files to
# @pattern: optional pattern to filter files to extract

sub extract_bundle {
    # get the arguments
    my ($bundle_path, $extract_path, $pattern) = @_;

    # check if the bundle file exists and die if not
    die "$bundle_path does not exist" if (! -f $bundle_path);

    # set the binary mode of the standard output to utf8
    binmode STDOUT, ":utf8";

    # open the input file handle
    my $IFH;
    open $IFH, "<", $bundle_path or die "Unable to open $bundle_path";
    binmode $IFH;

    # get the hash and list data from the input file handle
    my ($hash, $list) = get_table_data ($IFH);
    my ($str, $str2, $k);

    # loop through the list
    foreach my $h (@$list) {
        # skip if the name does not match the pattern
        next if ($h->{name} !~ /$pattern/);
        # print the file information for debugging purposes
        my @pname = ();
        foreach my $sh (sort keys %$h) {
            push @pname, $sh;
            push @pname, $h->{$sh};
        }
        print "  ", join (", ", @pname), "\n";

        # seek to the offset of the file in the input file handle
        seek ($IFH, $h->{offset}, SEEK_SET);
        # read the contents of the file from the input file handle
        read ($IFH, $str, $h->{length}) == $h->{length} or die "Error extracting from bundle";
        # get the file key
        my $key = get_file_key ($h->{offset});
        # decrypt the contents of the file
        for (my $i = 0; $i < $h->{length}; ++ $i) {
            $str2 .= chr (ord (substr ($str, $i, 1)) ^ $key);
        }
        # reset the contents of the file
        $str = '';

        # get the output name of the file
        my $out_name = $extract_path . '/' . $h->{name};

        # check the extension of the file
        if ($out_name =~ /\.cnv$/) {
            # get the key of the data
            $key = ord (substr ($str2, 0, 1));
            # convert the file based on the key of the data
            if ($key == 1) {
                convert_wav (\$str2);
                $out_name =~ s/cnv$/wav/;
            } elsif ($key == 24 || $key == 32) {
                convert_image (\$str2);
                $out_name =~ s/cnv$/tga/;
            } else {
                print "Bad data key ($key) in $out_name\n";
            }
        }
        # create any necessary subdirectories in the path of the output file
        for (my $i = 0; $i < length ($out_name); ++ $i) {
            if (substr ($out_name, $i, 1) eq '/' &&
            ! -d substr ($out_name, 0, $i)) {
                mkdir (substr ($out_name, 0, $i));
            }
        }

        # open the output file handle
        my $OFH;
        open $OFH, ">", $out_name or die "Unable to open $out_name";
        binmode $OFH;

        # write the decrypted contents of the file to the output file handle
        print $OFH $str2;

        # reset the contents of the decrypted file
        $str2 = '';
    }
}

sub convert_image { #cnv2tga
    my ($rstr) = @_;
    
    # Unpack values from the input image file format
    my ($bpp, $width, $height, $width2, $zero) = unpack ("CVVVV", substr ($$rstr, 0, 17));
    
    # Check if the two width values in the input file format match
    # If not, print a warning message
    if ($width != $width2) { print " *** Warning ----: Two width values disagree: $width $width2\n"; }
    
    # Check if the input image file has a supported bit depth
    # If not, raise an error
    if ($bpp != 24 && $bpp != 32) { die "BPP must be 24 or 32, not $bpp"; }
    
    # Check if the input image data has the correct length
    # If not, raise an error
    if ($width2 * $height * 4 + 17 != length ($$rstr)) { die "Data lengths disagree: ". ($width2 * $height * 4 + 17)." vs ".length ($$rstr); }
    
    # Check if the value of "zero" in the input image format is 0
    # If not, raise an error
    if ($zero != 0) { die "Nonzero value in final header block"; }

    # Prepare the output data
    my $outdat = '';
    $outdat .= pack ('ccc'.'vvc'.'vvvvcc', 0,0,2,    0,0,0, 
                     0,0,$width,$height, 32, 0x08);
    
    # Copy image data from the input image to the output data, flipping the rows
    for (my $r = $height - 1; $r >= 0; $r --) {
        for (my $c = 0; $c < $width2; ++ $c) {
            $outdat .= substr ($$rstr, 17 + 4 * ($r * $width2 + $c), 4);
        }
    }
    
    # Replace the input scalar with the output data
    $$rstr = $outdat;
}

sub convert_wav {#cnv2wav
    my ($rstr) = @_;

    # unpack values from the passed string
    my ($audio_fmt, $n_channels, $sample_rate, $byte_rate, 
        $block_align, $bits_per_sample, $extra_param_size, $subchunk_2_size) 
        = unpack ("vvVV"."vvvV", substr ($$rstr, 0, 22));

    # Check if the size of the passed string is equal to the subchunk size + 22
    if ($subchunk_2_size != length($$rstr) - 22) {
        # Print a warning message in case of mismatch
        print " *** Warning ----: Size mismatch: $subchunk_2_size vs. ", length($$rstr)-22, ".\n";
        #die "Size mismatch: $subchunk_2_size vs. ", length($$rstr)-22, ".";
    }

    # Check if the byte rate is equal to the product of sample rate, number of channels and bits per sample divided by 8
    if ($byte_rate != ($sample_rate * $n_channels * ($bits_per_sample/8))) {
        die "Byte rate mismatch: $byte_rate vs. ".
            ($sample_rate * $n_channels * ($bits_per_sample/8));
    }
    # Check if the block align is equal to the product of number of channels and bits per sample divided by 8
    if ($block_align != $n_channels * ($bits_per_sample/8)) {
        die "Block align mismatch: $block_align vs. ".
            $n_channels * ($bits_per_sample/8);
    }

    # Create a new packed wav file using the unpacked values
    # In this particular example, the format string is 'a4V'.'a4a4VvvVVvv'.'a4Va*'. It specifies the following structure:
    # A four-byte string ('RIFF')
    # A 4-byte unsigned integer ($subchunk_2_size + 36)
    # A four-byte string ('WAVE')
    # A four-byte string ('fmt ')
    # A 4-byte unsigned integer (16)
    # A 2-byte unsigned integer ($audio_fmt)
    # A 2-byte unsigned integer ($n_channels)
    # A 4-byte unsigned integer ($sample_rate)
    # A 4-byte unsigned integer ($byte_rate)
    # A 2-byte unsigned integer ($block_align)
    # A 2-byte unsigned integer ($bits_per_sample)
    # A four-byte string ('data')
    # A 4-byte unsigned integer ($subchunk_2_size)
    # A sequence of bytes, specified by substr ($$rstr, 22)
    $$rstr = pack ('a4V'.'a4a4VvvVVvv'.'a4Va*', 
        ('RIFF', $subchunk_2_size + 36), 
        ('WAVE', 'fmt ', 16, $audio_fmt, $n_channels, $sample_rate,
            $byte_rate, $block_align, $bits_per_sample), 
        ('data', $subchunk_2_size, substr ($$rstr, 22)));
}

sub rpatch_dir {
    my ($OFH, $full_path, $local_path, $h_table, $bundle_mtime) = @_;

    #print "rpatch_dir ($full_path  $local_path)\n";

    my $DIR;
    opendir ($DIR, $full_path);
    my @flist = (readdir ($DIR));
    closedir ($DIR);

    foreach my $fn (@flist) {
	next if ($fn eq '.' || $fn eq '..');
	my $full_name = $full_path.'/'.$fn;
	my $local_name = $local_path.$fn;
	my $in_name = $local_name;
	$in_name =~ s/\.wav$/\.cnv/;
	$in_name =~ s/\.tga$/\.cnv/;

	if ( -d "$full_path/$fn" ) {
	    rpatch_dir ($OFH, $full_name, $local_name.'/', $h_table, $bundle_mtime);
	} elsif (defined $h_table->{$in_name}) {
	    print " **** PATCH FILE  $local_name **** \n";
	    if ($bundle_mtime < -M "$full_path/$fn") {
		print "File is older; skipping\n";
		next;
	    }
	    undef $@;
	    eval {
		patch_file ($OFH, $full_name, $h_table->{$in_name});
	    };
	    print "Error: $@\n" if $@;
	} else {
	    print "Ignoring file $local_name\n";
	}
    }
}

sub get_file_key {
    # shift operator removes and returns the first element of the list
    my $offset = shift;
    # bitwise right shift to divide the offset by 2
    # bitwise and operation to mask the result with 0xff (255)
    # bitwise or operation to set the 8th bit to 1
    return (($offset >> 1) & 0xff) | 0x08;
}
	
sub read_file_in {
    my ($fn) = @_;

    my $IFH;
    open $IFH, "<", $fn;
    binmode $IFH;
    local $/;
    my $in_dat = <$IFH>;

    if ($fn =~ /\.wav$/) {
	print "Converting wav to cnv\n";
	my ($chunk_id, $chunk_size, $format,
	    ($sub_1_id, $sub_1_size, $aud_fmt, $n_channels, $s_rate, 
	     $byte_rate, $block_align, $bits_per_sample),
	    $sub_2_id, $sub_2_size) =
		unpack ('a4Va4'.'a4VvvVVvv'.'a4V', substr ($in_dat, 0, 44));
	if ($chunk_id ne 'RIFF' || $format ne 'WAVE' ||
	    $sub_1_id ne 'fmt ' || $sub_1_size != 16 || 
	    $sub_2_id ne 'data' || $chunk_size != $sub_2_size + 36) {
	    die "Bad headers on .wav";
	}
	if ($sub_2_size != length($in_dat) - 44) {
	    print " *** Warning $fn: Size mismatch: $sub_2_size vs. ", length($in_dat)-44, ".\n";
	    #die "Size mismatch: $subchunk_2_size vs. ", length($$rstr)-44, ".";
	}
	die "Can only use 44100 bps wavs, not $s_rate" if ($s_rate != 44100); 
	if ($byte_rate != ($s_rate * $n_channels * ($bits_per_sample/8))) {
	    die "Byte rate mismatch: $byte_rate vs. ".
		($s_rate * $n_channels * ($bits_per_sample/8));
	}
	if ($block_align != $n_channels * ($bits_per_sample/8)) {
	    die "Block align mismatch: $block_align vs. ".
		$n_channels * ($bits_per_sample/8);
	}
	return \ (pack ("vvVV"."vvvV", 
		       $aud_fmt, $n_channels, $s_rate, $byte_rate, 
		       $block_align, $bits_per_sample, 0, $sub_2_size).
		  substr ($in_dat, 44));
    } elsif ($fn =~ /\.tga$/) { # tga2cnv
	print "Converting tga to cnv\n";
	my ($arg_1, $arg_2, $arg_3,  $arg_4, $arg_5, $arg_6,
	    $arg_7, $arg_8,  $width, $height, $bpp, $trans) = 
		unpack ('cccvvc'.'vvvvcc', substr ($in_dat, 0, 18));
	if ($arg_1 != 0 || $arg_2 != 0 || $arg_3 != 2 || 
	    $arg_4 != 0 || $arg_5 != 0 || $arg_6 != 0 || 
	    $arg_7 != 0 || $arg_8 != 0) {
	    die "Bad headers on .tga";
	}
	die ".tga not 32 bpp" if ($bpp != 32);
	die ".tga has wrong transparency" if ($trans != 0x08);

	my $out_dat = pack ('CVVVV', $bpp, $width, $height, $width, 0);
	for (my $r = $height - 1; $r >= 0; -- $r) {
	    for (my $c = 0; $c < $width; ++ $c) {
		$out_dat .= substr ($in_dat, 18 + 4 * ($r * $width + $c), 4);
	    }
	}
	return \$out_dat;
    } else {
	print "Not converting\n";
	return \$in_dat;
    }
}

sub hexify_str {
    return join (' ', map { sprintf ("%02x", $_) } unpack ('C*', $_[0]));
}

sub patch_file {
    my ($OFH, $in_file_name, $ftable) = @_;

    print "Updating file ", join (" ", %$ftable), ".\n";

    my $r_data = read_file_in ($in_file_name);
    my $dat_len = length ($$r_data);

    if ($dat_len <= $ftable->{length}) {
	print "Seeking to ", $ftable->{offset}, ".\n";
	seek ($OFH, $ftable->{offset}, SEEK_SET) or die "Unable to seek: $!";
    } else {
	print "Seeking to end\n";
	seek ($OFH, 0, SEEK_END) or die "Unable to seek: $!";
    }
    my $new_offset = tell ($OFH);
    print "Updating at $new_offset\n";

    my $key = get_file_key ($new_offset);
    my $out_data = '';
    for (my $i = 0; $i < $dat_len; ++ $i) {
	$out_data .= chr (ord (substr ($$r_data, $i, 1)) ^ $key);
    }
    my $c_printed = print $OFH $out_data;
    #$c_printed == $dat_len or print "Write size mismatch: $dat_len to print, $c_printed printed\n";
    $c_printed or die "Error writing output data (c_printed = $c_printed): $!";

    if ($dat_len != $ftable->{length} || $new_offset != $ftable->{offset}) {
	my $f_index = 268*$ftable->{index} + 260 + 2;
	seek ($OFH, $f_index, SEEK_SET) or die "Unable to seek: $!";
	print "Updating file_table from ", $ftable->{length}, ", ", $ftable->{offset}, "  to  $dat_len, $new_offset\n";
	my $old_block;
	read ($OFH, $old_block, 8);
	#print "old was (", join (" ", unpack ('c8', $old_block)), ")\n";
	#print "old was (", join (' ', map { sprintf ("%02x", abs($_)) } unpack ('c8', $old_block)), ")\n";
	print "old was (", hexify_str ($old_block), ")\n";

	my $i = print $OFH decrypt_file_table_block ($f_index-2, pack ('VV', $dat_len, $new_offset));

	$old_block = decrypt_file_table_block ($f_index-2, pack ('VV', $ftable->{length}, $ftable->{offset}));
	#print "old shld(", join (' ', map { sprintf ("%02x", abs($_)) } unpack ('c8', $old_block)), ")\n";
	print "old shld(", hexify_str ($old_block), ")\n";

	#print "old decr(", hexify_str (pack ('VV', $ftable->{length}, $ftable->{offset}));

	$old_block = decrypt_file_table_block ($f_index-2, pack ('VV', $dat_len, $new_offset));
	#print "new is  (", join (" ", unpack ('c8', $old_block)), ")\n";
	print "new is  (", hexify_str ($old_block), ")\n";
	#print "new is  (", join (' ', map { sprintf ("%02x", abs($_)) } unpack ('c8', $old_block)), ")\n";
	#print "new decr(", hexify_str (pack ('VV', $ftable->{length}, $ftable->{offset}));

	$ftable->{offset} = $new_offset;
	$ftable->{length} = $dat_len;
	#$i == 1 or die "Error updating file table: $!";
    }
}
	
sub patch_bundle {
    my ($orig_bundle, $new_bundle, $root) = @_;

    my ($IFH, $OFH);
    
    #system ('cp', $orig_bundle, $new_bundle) && die "Unable to copy bundle";
    #my $i = system ('cp', $orig_bundle, $new_bundle);
    #print "copy -> $i\n";

    #open ($IFH, "<", $orig_bundle) or die "Unable to open $orig_bundle for patching";
    my $mtime = -M $new_bundle;
    open ($OFH, "+<", $new_bundle) or die "Unable to open $new_bundle for writing: $!";
    binmode $OFH;

    my ($h_table, $l_table) = get_table_data ($OFH);

    #show_file_table ($orig_bundle);

    rpatch_dir ($OFH, $root, '', $h_table,   $mtime);
}

#patch_bundle ('../Daybreak/daybreak01.dat', 'alt-bundle', 'new_01');
#patch_bundle ('../Daybreak/daybreak00.dat', 'alt-bundle', 'new_00');
#eval { patch_bundle ('../Daybreak/backup_daybreak00.dat', 'daybreak00.dat', 'new_00'); };

my $action = shift @ARGV;

if ($action eq '--update') {
    eval { patch_bundle ('../Daybreak/backup_daybreak00.dat', 'daybreak00.dat', 'new_00'); };
} elsif ($action eq '--update1') {
    eval { patch_bundle ('/media/sdb1/Program Files/07th Expansion/daybreak/orig.daybreak00.dat', '/media/sdb1/Program Files/07th Expansion/daybreak/daybreak00.dat', '/media/sdb1/Program Files/07th Expansion/daybreak/new_00'); };
} elsif ($action eq '--extract') {
    if (@ARGV < 2 || @ARGV > 3) {
	die "Usage: perl bundle-tools --extract <bundle> <destination> <files>";
    }
    my $bundle_name = shift @ARGV;
    my $dest_dir = shift @ARGV;
    my $pattern = '';
    if (@ARGV) { $pattern = shift @ARGV; }
    eval { extract_bundle ($bundle_name, $dest_dir, $pattern); }
} elsif ($action eq '--list') {
    eval { list_bundle ($ARGV[0], $ARGV[1]); }
} else {
    $@ = "Bad action: $action\n";
}

print "Error: $@" if $@;

#show_file_table ('../Daybreak/daybreak00.dat');
    
