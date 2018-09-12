object BracketPush {
  def isPaired(text: String): Boolean = {
    def isPairedRec(openBrackets: String, text: String): Boolean = {
      if (text.isEmpty)
        openBrackets.isEmpty
      else
        openBrackets.headOption match {
          case Some('{') if text.head == '}' => isPairedRec(openBrackets.tail, text.tail)
          case Some('[') if text.head == ']' => isPairedRec(openBrackets.tail, text.tail)
          case Some('(') if text.head == ')' => isPairedRec(openBrackets.tail, text.tail)
          case None | Some(_) if "{}[]()".contains(text.head) => isPairedRec(text.head + openBrackets, text.tail)
          case _ => isPairedRec(openBrackets, text.tail)
        }
    }
    isPairedRec("", text)
  }


}
