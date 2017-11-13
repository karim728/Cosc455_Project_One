object Patterns {

  val beginPattern = """(\\[b,B][E,e][G,g][I,i][N,n])""".r
  val endPattern = """(\\[E,e][n,N][D,d])""".r
  val textPattern = """([A-Za-z0-9\,\.\"\:\?_\/]+)""".r

  // variable use
  val variableUsePattern = """(\\[U,u][S,s][E,e]\[[a-zA-z_0-9]+\])""".r // [\USE[


  //title pattern
  val titlePattern = """(\\[T,t][I,i][T,t][L,l][E,e]\[[a-zA-Z_0-9\s\']+\])""".r

  // variable define
  val variableDefPattern = """(\\[D,d][E,e][F,f]\[[a-zA-z_0-9]+\s?\=\s?[a-zA-z_0-9]+\])""".r   //\DEF[


  //Italics
  val italicsPattern = """(\*[a-zA-Z_0-9\s]+\*)""".r// * text *

  // Bold
  val boldPattern = """(\*\*[a-zA-Z_0-9\s]+\*\*)""".r// ** text **

  // Headings
  val headingPattern = """(\#[a-zA-Z_0-9\s]+)""".r

  val commentPattern = """(\[.+\])""".r
  val addressPattern = """(\(.+\))""".r

  val imagePattern = """(\!\s?\[.+\]\s?\(.+\)?\s?)""".r // not sure if using this to check if it's correct or slicing it up and compare versus previous patterns if better

  val newLinePattern = """(\\\\)""".r


  val linkPattern = """(\[.+\]\(.+\))""".r

  val listPattern = """(\+.?)""".r

  val paragraphBeginPattern = """(\\[P,p][A,a][R,r][A,a][B,b])""".r
  val paragraphEndPattern = """(\\[P,p][A,a][R,r][A,a][E,e])""".r
  //    //case '+' => getList()
  //case '\\' => identifySlash()
  //case '(' => getAddress()
  //case '!' => getImage()









  //  The only allowed plain text in our language is: A-Z, a-z, 0-9, commas, period, quotes, colons, question marks, underscores and forward slashes.
  val generalTextPattern = """([A-Za-z0-9\,\.\"\:\?_\/])""".r

  val generalMultiTextPattern = """[A-Za-z0-9\,\.\"\:\?_\/]+""".r
}
