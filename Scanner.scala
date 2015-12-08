package lang

class Scanner {
  def scanner(text: String): List[Token] = {
    def scan(index: Int): List[Token] = {
      var i = index
      if(i == text.length) return List.empty[Token]
      val c = text(i)
      if(c == '(') LParen +: scan(i+1)
      else if(c == ')') RParen +: scan(i+1)
      else if(c == '[') LBrack +: scan(i+1)
      else if(c == ']') RBrack +: scan(i+1)
      else if(c == '<') LAngle +: scan(i+1)
      else if(c == '>') RAngle +: scan(i+1)
      else if(c == '{') LCurly +: scan(i+1)
      else if(c == '}') RCurly +: scan(i+1)
      else if(c == ',') Comma +: scan(i+1)
      else if(c == '.') scanDigits(i)

      else if(c == '/') Slash +: scan(i+1)
      else if(c == '+') Plus +: scan(i+1)
      else if(c == '-') Minus +: scan(i+1)
      else if(c == '%') Percent +: scan(i+1)
      else if(c == '*') Ast +: scan(i+1)
      else if(c == '!') Exclamation +: scan(i+1)
      else if(c == '=') Equals +: scan(i+1)
      else if(c == ':') Colon +: scan(i+1)
      else if(c.isDigit) scanDigits(i)
      else if(c == '\"') scanStr(i)
      else if(c == '\'') scanChr(i)
      else if(c == '#') scanComment(i+1)
      else if(c.isLetter) scanLettersAndNumbers(i)     
      else if(c.isWhitespace) scan(i+1)
      else throw new InvalidCharacterException("Invalid char: " + c)
    }
    
    def scanComment(index: Int): List[Token] = {
      //Skips over the comment that begins at index.
      //Returns the scan of the character that follows the token.
      //For single line comments (#), skips to next new line.
      if(text(index) == '*') scanBlockComment(index + 1)
      else {
        var i = index
        while(i < text.length){
          if(i < text.length-1 && text(i) == '\n') return scan (i+1) 
          i += 1
        } 
        scan(i)
      }
    }

    def scanBlockComment(index: Int): List[Token] = {
      var i = index
      while(i < text.length){
        if (text(i) == '*' && i+1 < text.length && text(i+1) == '#') return scan(i+2)
        else if (text(i) == '*' && i+1 == text.length && text(i+1) == '#') return List.empty[Token]
        else i += 1
      } 
      scan(i)
    }
    
    def scanDigits(index: Int): List[Token] = {
      var i = index
      var num = ""
      while(i < text.length && (text(i).isDigit || (text(i) == '.' && !num.contains('.')))){
        num += text(i)
        i += 1
      }
      if(num.contains('.')) Flt(num.toFloat) +: scan(i)
      else Integer(num.toInt) +: scan(i)
    }
    
    def scanStr(index: Int): List[Token] = {
      var i = index + 1
      while(i < text.length && (text(i) != '\"')) i += 1
      if(i > text.length) throw new StringScanException("Expected closing \".")
      else if (text(i) == '\"') {
        val s = text.substring(index + 1, i)
        if (s.contains("\n")) throw new Exception("Literal newline character forbidden inside string.")
        Str(s) +: scan(i+1)
      }
      else throw new StringScanException("Unexpected end to string")
    }
    
    def scanChr(index: Int): List[Token] = {
      val i = index + 1
      if(i >= text.length - 1) throw new CharScanException("Expected closing \'.")
      val c = text(i)
      if(text(i+1) != '\'') throw new CharScanException("Char must be of length 1.")
      Chr(c) +: scan(i+2)
    }
    
    def scanLettersAndNumbers(index: Int): List[Token] = {
      var i = index
      var word = ""
      while(i < text.length && (text(i).isLetterOrDigit || text(i) == '_')){
        word += text(i)
        i += 1
      }
      if(word == "True") Bool(true) +: scan(i)
      else if(word == "False") Bool(false) +: scan(i)
      else if(word == "and") AndTok +: scan(i)
      else if(word == "or") OrTok +: scan(i)
      else if(word == "not") NotTok +: scan(i)
      else if(word == "for") ForTok +: scan(i)
      else if(word == "if") IfTok +: scan(i)
      else if(word == "else") {
        if (i + 1 < text.length - 3 && text(i).isWhitespace && text(i+1)== 'i' && text(i+2) == 'f') ElseIfTok +: scan(i+3)
        else ElseTok +: scan(i) 
      }
      else if(word == "while") WhileTok +: scan(i)
      else if(word == "return") ReturnTok +: scan(i)
      else if(word == "print") PrintTok +: scan(i)
      else if(word == "to") ToTok +: scan(i)
      else if(word == "until") UntilTok +: scan(i)
      else if(word == "in") InTok +: scan(i)
      /*
      else if(word == "Int") IntTok +: scan(i)
      else if(word == "Void") VoidTok +: scan(i)
      else if(word == "Char") CharTok +: scan(i)
      else if(word == "Double") DoubleTok +: scan(i)
      else if(word == "Boolean") IntTok +: scan(i)
      */
      else if(word == "BEGIN") BeginTok +: scan(i)
      else if(word == "END") EndTok +: scan(i)
      else if(word == "Array") ArrayTok +: scan(i)
      else Symbol(word) +: scan(i)
    }
    
    scan(0)
  }
}