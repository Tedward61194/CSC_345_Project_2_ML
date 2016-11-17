(*
 * Author: Ted Segal
 * Date: 11/17/2016
 * Assignment: ML Homework
 *)

(*
 * flip function flips pairs of a list.
 * eg. flip [1,2,3,4,5,6] would return [2,1,4,3,6,5]
 * No known bugs
 *)

fun getFirstLetter (f) = hd (explode f);

fun flip (x::y::xs) = y::x::flip xs
  | flip _  = [];

(*
 * deleteIth accepts a list and an index and deletes the element at the index in the list 
 * Known Bug: There seems to be an off-by-one error that I can't seem to get rid of.
 * Whenever I try to subtract more fromt he index-input, there deletion stops working
 *)


fun deleteIth (nil, _) = nil
  | deleteIth (x::xs, 0) = xs
  | deleteIth (x::xs, i) = x::deleteIth(xs, i-1);

(*
 * beginsWithAVowelHelpers returns equal if a string that begining
 * with a particular vowel is passed as the param)
 *)

fun beginsWithAVowelHelper (s) =
  (Char.compare (getFirstLetter (s), #"a"));
      
fun beginsWithAVowelHelper2 (s) =
  (Char.compare (getFirstLetter (s), #"e"));

fun beginsWithAVowelHelper3 (s) =
  (Char.compare (getFirstLetter (s), #"i"));

fun beginsWithAVowelHelper4 (s) =
  (Char.compare (getFirstLetter (s), #"o"));

fun beginsWithAVowelHelper5 (s) =
  (Char.compare (getFirstLetter (s), #"u"));

(*
 * beginswithavowel will return true if the parameter returns "equal" to one of the helper functions
 *)
fun beginsWithAVowel (s) =
  beginsWithAVowelHelper("apple") = beginsWithAVowelHelper(s)
  orelse beginsWithAVowelHelper("apple") = beginsWithAVowelHelper2(s)
  orelse beginsWithAVowelHelper("apple") = beginsWithAVowelHelper3(s)
  orelse beginsWithAVowelHelper("apple") = beginsWithAVowelHelper4(s)
  orelse beginsWithAVowelHelper("apple") = beginsWithAVowelHelper5(s);

(* concat yay at the end of the word, called when starts with vowel*)
fun pigHelper1 (word) = word^"yay";

(* pigHelper2 brings the first letter to the end of the word,
 *then calls helper 3 which deletes the first, then if its not
 *a vowel calls pigHelper2 again, and if it is calls pig helper 1
*)
fun pigHelper2 (word) =
  let
      val newWord = word^(Char.toString (getFirstLetter(word)))
      fun pigHelper3 (word) =
	let
	    val broken = explode word
	    val newList = deleteIth(broken, 0)
	    val newWord = implode newList
	in
	    pigHelper2 (newWord)
	end
  in
      if (beginsWithAVowel (newWord)) then pigHelper1 (newWord)
      else pigHelper3 (newWord)
  end;

(* should we start with pigHelper1 or 2*)
fun piglatinize (word) =
  if (beginsWithAVowel (word)) then pigHelper1 (word)
  else pigHelper2 (word);
