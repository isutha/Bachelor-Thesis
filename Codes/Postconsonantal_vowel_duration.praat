######################################################
#						     #
#  Script to extract postconsonantal vowel durations #
#						     #
######################################################


# where are my .TextGrid files?
dirTextGrid$ = "C:\Users\ishwa\OneDrive - Universität Zürich UZH\Desktop\Uni\Uni_ZH\Computerlinguistik\BA_Arbeit\to_extract\pt_extr\"

# where should my output go?
outfile$ = "C:\Users\ishwa\OneDrive - Universität Zürich UZH\Desktop\Uni\Uni_ZH\Computerlinguistik\BA_Arbeit\praat\processed_files\output_pt.txt"

# create outputfile (if already exists, delete it)
deleteFile: outfile$

appendFile: outfile$, "path" + tab$ + "Consonant_before_vowel" + tab$ + "Vowel" + tab$ + "Start_time_V(ms)" + tab$
appendFile: outfile$, "End_time_V(ms)" + tab$ + "Duration_V(ms)" + tab$ + "Phone_after_V" + tab$ + "Duration_phone_after_V(ms)" + tab$
appendFileLine: outfile$, "Word" + tab$ + "Starttime_word(ms)" + tab$ + "Endtime_word(ms)" + tab$ + "Word_Duration(ms)" + tab$ + "Speaker_rate"

# create a list of all TextGrids
strings = Create Strings as file list: "list", dirTextGrid$ + "*.TextGrid"

number_files = Get number of strings

for index_file from 1 to number_files
	selectObject: strings
	filename$ = Get string: index_file
	basename$ = filename$ - ".TextGrid"
	#pauseScript: filename$
	Read from file: dirTextGrid$ + filename$

	# get number of intervals. Here 2 because phone tier is 2nd
	phoneCount= Get number of intervals: 2
	#pauseScript:"check"
	
	start_interval = 1
	labelphone_st$ = Get label of interval: 2, start_interval

	if labelphone_st$ == ""
		start_interval = 2
		labelphone_st$ = Get label of interval: 2, start_interval
	endif
	utterance_start = Get start time of interval: 2, start_interval
	
	end_interval = phoneCount
	labelphone_end$ = Get label of interval: 2, end_interval
	if labelphone_end$ == ""
		end_interval = end_interval - 1
		labelphone_end$ = Get label of interval: 2, end_interval
	endif

	utterance_end = Get end time of interval: 2, end_interval

	# utterance dur is in seconds
	utterance_dur = utterance_end - utterance_start

	#pauseScript: utterance_dur, " s"

	# calculate number of spoken phones (add one bc end phone is not inclusive)
	phones = end_interval - start_interval + 1 

	# phones per second
	spk_rate = phones / utterance_dur

	for p from 1 to phoneCount
		selectObject: "TextGrid " + basename$
		phone$ = Get label of interval: 2, p
		
		if index_regex(phone$, "[ptkbdɡg]") and length(phone$) = 1
			phone_start = Get start time of interval: 2, p
			phone_end = Get end time of interval: 2, p
			phone_dur = phone_end - phone_start

			word_int = Get interval at time: 1, phone_start
			word$ = Get label of interval: 1, word_int
			word_start = Get start time of interval: 1, word_int
			word_end = Get end time of interval: 1, word_int
			word_dur = word_end - word_start

			time_point = phone_start + (0.5 * phone_dur)
			#pauseScript: "check here"
			utt_perc = (time_point - utterance_start) / utterance_dur
		
		if utt_perc < 0.5
			nextPhoneInt = p + 1
			afterVowelInt = p + 2

			#if not afterVowelInt > phones	
			# check for vowel
			nextPhone$ = Get label of interval: 2, nextPhoneInt
			afterVowel$ = Get label of interval: 2, afterVowelInt

		# Check if next phone is a vowel
		if index_regex(nextPhone$, "[iɨɪʊɯuaæɑ]") and length(nextPhone$) = 1 and index_regex(afterVowel$, "[ptksʃ]") and length(afterVowel$) = 1
			nextPhone_start = Get start time of interval: 2, nextPhoneInt
			nextPhone_end = Get end time of interval: 2, nextPhoneInt
			nextPhone_dur = nextPhone_end - nextPhone_start
			time_point_next = nextPhone_start + (0.5 * nextPhone_dur)

			afterVowel_start = Get start time of interval: 2, afterVowelInt
			afterVowel_end = Get end time of interval: 2, afterVowelInt
			afterVowel_dur = afterVowel_end - afterVowel_start
			time_point_consonant = afterVowel_start + (0.5 * afterVowel_dur)

			# calculate the utterance percentage with the vowel and the following consonant
			vowel_perc = (time_point_next - utterance_start) / utterance_dur
			#consonant_perc = (time_point_consonant - utterance_start) / utterance_dur

			# Get word intervals for the next phone and the after-vowel phone
			nextPhone_word_int = Get interval at time: 1, nextPhone_start
			afterVowel_word_int = Get interval at time: 1, afterVowel_start

		if nextPhone_word_int == word_int and afterVowel_word_int == word_int and vowel_perc < 0.5
			phone_start = phone_start
			phone_start$ = fixed$(phone_start, 3)
			nextPhone_start = nextPhone_start * 1000
			nextPhone_start$ = fixed$(nextPhone_start, 3)
			nextPhone_end = nextPhone_end * 1000
			nextPhone_end$ = fixed$(nextPhone_end, 3)
			nextPhone_dur = nextPhone_dur * 1000
			nextPhone_dur$ = fixed$(nextPhone_dur, 3)
			afterVowel_start = afterVowel_start * 1000
			afterVowel_start$ = fixed$(afterVowel_start, 3)
			afterVowel_end$ = fixed$(afterVowel_end, 3)
			afterVowel_dur = afterVowel_dur * 1000
			afterVowel_dur$ = fixed$(afterVowel_dur, 3)
			word_start = word_start * 1000
			word_start$ = fixed$(word_start, 3)
			word_end = word_end * 1000
			word_end$ = fixed$(word_end, 3)
			word_dur = word_dur * 1000
			word_dur$ = fixed$(word_dur, 3)
			spk_rate$ = fixed$(spk_rate, 3)

			appendFile: outfile$, basename$, tab$, phone$, tab$, nextPhone$, tab$, nextPhone_start$, tab$
			appendFile: outfile$, nextPhone_end$, tab$, nextPhone_dur$, tab$, afterVowel$, tab$, afterVowel_dur$, tab$
			appendFileLine: outfile$, word$, tab$, word_start$, tab$, word_end$, tab$, word_dur$, tab$, spk_rate$
		endif
		endif
		endif
		endif
	endfor

	select all
	minusObject: "Strings list"
	Remove
endfor