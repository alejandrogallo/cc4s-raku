use v6;

unit module Cc4s;

sub TensorReader( Str :data($data)
                , Str :file($file) = ""
                , Str :mode($mode) where * (elem) < binary text > = "text"
                ) {
  qq!
TensorReader [
  (data $data) % data to store in
  { $file ?? "(file \"$file\")" !! "" }
  (mode "$mode") % format of the tensor file
][].
!
}
