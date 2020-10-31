object Cipher{
  /** Bit-wise exclusive-or of two characters */
  def xor(a: Char, b: Char) : Char = (a.toInt ^ b.toInt).toChar

  /** Print ciphertext in octal */
  def showCipher(cipher: Array[Char]) =
    for(c <- cipher){ print(c/64); print(c%64/8); print(c%8); print(" ") }

  /** Read file into array */
  def readFile(fname: String) : Array[Char] = 
    scala.io.Source.fromFile(fname).toArray

  /** Read from stdin in a similar manner */
  def readStdin() = scala.io.Source.stdin.toArray

  /** Encrypt plain using key; can also be used for decryption */
  def encrypt(key: Array[Char], plain: Array[Char]) : Array[Char] = {
    var n = plain.size
    var k = key.size
    var cipher = new Array[Char](n)	
    var i = 0
    while (i < n){ 
      // we use key(i%k) to make sure that the indices of key
      // do not fall outside the size of key, k. Whenever we reach
      // i = k we go back cyclically to i = 0, i.e i%k.
      cipher(i) = xor (key(i%k), plain(i))
      i += 1
    }
    cipher
}

  /** 
  tr02[~/Practicals/IP/prac1]$ 
  scala Cipher -encrypt RUDOLF santa | scala Cipher -decrypt RUDOLF
  Dear Santa, Please bring me a new bike for Christmas, love John
*/

  /** See if the keyChars contains a repetition of at least two characters */
  def checkRep (keyChars : Array[Char], j: Int) : Boolean = {
    var k = keyChars.size
    var i = j
    var eq = true
    // check keyChars[0..k-j) == keyChars[j..k)
    while (i < k && eq) {
      // if we find a mismatch, the equality can't hold
      if (keyChars(i-j) != keyChars(i)) eq = false
      i += 1
    }
    eq
}
  
  /** Try to decrypt ciphertext, using crib as a crib */
  def tryCrib(crib: Array[Char], ciphertext: Array[Char]) : Unit = {
    var n = ciphertext.size
    var k = crib.size
    var start = 0
    //place the crib at each start position that is possible
    while (start < n - k + 1){
      //keyChars stores the entries of the supposed key in
      //some order and with possible repetitions.
      var keyChars = new Array[Char](k)
      var i = 0
      while (i < k){
        keyChars(i) = xor (ciphertext(start+i), crib(i))
        //since ciphertext(start+i) = crib(i)⊕keyChars(i)
        i += 1
      }
      var j = 1
      //check for repetitions of at least 2 entries in keyChars
      while (!checkRep(keyChars,j) && j <= k-2) {j += 1}
      // if the loop ended because we found a repetition
      if (checkRep(keyChars,j)) { // we have a repetition and know that the key's length is j
        var key = new Array[Char](j)
        var first = j - start % j
        // We know that the key has length j. It repeated cyclically
        // (start div j) times in the ciphertext before reaching start.
        // Hence (start % j) + first = j. Example:
        // Dear Santa, Please bring me a new bike for Christmas, love John
        // RUDOLFRUDOLFRUDOLFRUDOLFRUDOLFRUDOLFRUDOLFRUDOLFRUDOLF
        //                                            s
        //                                            123456
        i = 0
        while (i < j){
          print(keyChars((first + i) % j)); 
          key(i) = keyChars((first + i) % j); 
          i += 1
        }
        println()
        //get back the original text since we have the key and thanks to
        //⊕ cancellation property
        print(new String (encrypt(key,ciphertext)))
        return 
      }
      start += 1
	}
}

  /**
  tr02[~/Practicals/IP/prac1]$ scala Cipher -encrypt FALALA santa | scala Cipher -crib Christmas
  FALALA
  Dear Santa, Please bring me a new bike for Christmas, love John

  tr02[~/Practicals/IP/prac1]$ scala Cipher -encrypt PINGPONG santa | scala Cipher -crib Christmas,
  PINGPONG
  Dear Santa, Please bring me a new bike for Christmas, love John

*/

  /** The first statistical test, to guess the length of the key */
  def crackKeyLen(ciphertext: Array[Char]) : Unit = {
    val n = ciphertext.length	
    var shift = 1
    while (shift <= 40){ //try various shifts for matches
      var cnt = 0
      var i = 0
      while (i < n-shift){ //count the numbers of matches for the current shift
        if(ciphertext(i)==ciphertext(shift+i)) 
          cnt += 1
        i += 1

      }
      println(shift+": "+cnt)
      shift += 1
    }
}

  /** The second statistical test, to guess characters of the key. */
  def crackKey(klen: Int, ciphertext: Array[Char]) : Unit = {
    val n = ciphertext.length
    var i = 1	
    var s = i * klen
    while (i < n/klen) { //try shifts which are multiples of klen
      s = i * klen
      var j = 0
      while (j < n-s){
        var guess = xor(' ', ciphertext(j))
        if(ciphertext(j)==ciphertext(s+j) &&  guess >= 32 && guess <=127)
        // if we find a match we guess that this corresponds to a space
        // and we print the required key character if it is printable.
          println(j%klen+" "+guess)
          // we know that the key has length klen. It repeated
          // cyclically (j div klen) times in the ciphertext before reaching j
          // and hence key must have guess on position (j % klen).
        j += 1
      }
      i += 1
    }
}

  /**
  tr02[~/Practicals/IP/prac1]$ scala Cipher -crackKey 9 private1 | sort -n | uniq -c | awk '$1 > 10'
     55 0 P
     21 1 E
     21 2 M
    120 3 B
     78 4 E
     78 5 R
    105 6 L
     45 7 E
     66 8 Y
*/

  /**
  tr02[~/Practicals/IP/prac1]$ scala Cipher -decrypt PEMBERLEY private1
  Be not alarmed, Madam, on receiving this letter, by the
  apprehension of its containing any repetition of those
  sentiments, or renewal of those offers, which were last night
  so disgusting to you.  I write without any intention of paining
  you, or humbling myself, by dwelling on wishes, which, for the
  happiness of both, cannot be too soon forgotten; and the effort
  which the formation and the perusal of this letter must
  occasion should have been spared, had not my character required
  it to be written and read.  You must, therefore, pardon the
  freedom with which I demand your attention; your feelings, I
  know, will bestow it unwillingly, but I demand it of your
  justice.

*/

  /**
  tr02[~/Practicals/IP/prac1]$ scala Cipher -crackKey 8 private2 | sort -n | uniq -c | awk '$1 > 10'
     55 0 H
     36 1 O
     28 2 G
     45 3 W
     15 4 A
     15 5 R
     21 6 T
     45 7 S

*/

  /**
  tr02[~/Practicals/IP/prac1]$ scala Cipher -decrypt HOGWARTS private2
  HOGWARTS SCHOOL of WITCHCRAFT and WIZARDRY
  Headmaster: Albus Dumbledore (Order of Merlin, First Class, Grand
  Sorc., Chf. Warlock, Supreme Mugwump, International Confed. of
  Wizards)
  Dear Mr Potter,
  We are pleased to inform you that you have been accepted at Hogwarts
  School of Witchcraft and Wizardry. Please find enclosed a list of all
  necessary books and equipment. Term begins on 1 September. We await
  your owl by no later than 31 July.
  Yours sincerely, Minerva McGonagall. Deputy Headmistress
*/

/** The main method just selects which piece of functionality to run */
  def main(args: Array[String]) = {
    // string to print if error occurs
    val errString = 
      "Usage: scala Cipher (-encrypt|-decrypt) key [file]\n"+
      "     | scala Cipher -crib crib [file]\n"+
      "     | scala Cipher -crackKeyLen [file]\n"+
      "     | scala Cipher -crackKey len [file]"

    // Get the plaintext, either from the file whose name appears in position
    // pos, or from standard input
    def getPlain(pos: Int) = 
      if(args.length==pos+1) readFile(args(pos)) else readStdin

    // Check there are at least n arguments
    def checkNumArgs(n: Int) = if(args.length<n){println(errString); sys.exit}

    // Parse the arguments, and call the appropriate function
    checkNumArgs(1)
    val command = args(0)
    if(command=="-encrypt" || command=="-decrypt"){
      checkNumArgs(2); val key = args(1).toArray; val plain = getPlain(2)
      print(new String (encrypt(key,plain)))
    }
    else if(command=="-crib"){
      checkNumArgs(2); val key = args(1).toArray; val plain = getPlain(2)
      tryCrib(key, plain)
    }
    else if(command=="-crackKeyLen"){
      checkNumArgs(1); val plain = getPlain(1)
      crackKeyLen(plain)
    }      
    else if(command=="-crackKey"){
      checkNumArgs(2); val klen = args(1).toInt; val plain = getPlain(2)
      crackKey(klen, plain)
    }
    else println(errString)
  }
}
