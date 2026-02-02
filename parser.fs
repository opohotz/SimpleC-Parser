//
// Parser for SimpleC programs.  This component checks 
// the input program to see if it meets the syntax rules
// of SimpleC.  The parser returns a string denoting
// success or failure. 
//
// Returns: the string "success" if the input program is
// legal, otherwise the string "syntax_error: ..." is
// returned denoting an invalid SimpleC program.
//
// ANJALI VISWAN
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.
  //

  let private beginswith (pattern: string) (literal: string) =
    literal.StartsWith (pattern)

  //
  // eat
  //
  let private eat expected_token (tokens: string list) =
    //
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation
    // at the first error.
    //
    let next_token = List.head tokens

    if expected_token = next_token then  
      List.tail tokens
    elif beginswith expected_token next_token then
      List.tail tokens
    elif expected_token = "identifier:" then
      failwith ("expecting identifier, but found " + next_token)
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)

  //
  // expr_value
  // validates grammar in the 3 variable types: int string bool
  let rec private expr_value tokens =
    let next_token = List.head tokens
    if beginswith "identifier:" next_token || 
       beginswith "int_literal:" next_token || 
       beginswith "str_literal:" next_token ||
       next_token = "true" || 
       next_token = "false" then
      List.tail tokens
    else
      failwith ("expecting identifier or literal, but found " + next_token)

  //
  // expr_op
  // mathematical operations reads
  let rec private expr_op tokens =
    let next_token = List.head tokens
    if next_token = "+" || next_token = "-" || next_token = "*" || 
       next_token = "/" || next_token = "^" || next_token = "<" || 
       next_token = "<=" || next_token = ">" || next_token = ">=" || 
       next_token = "==" || next_token = "!=" then
      List.tail tokens
    else
      failwith ("expecting expression operator, but found " + next_token)

  //
  // expr
  // for calculating logical expressions
  let rec private expr tokens =
    let T2 = expr_value tokens
    let next_token = if not (List.isEmpty T2) then List.head T2 else "$"
    if next_token = "+" || next_token = "-" || next_token = "*" || 
       next_token = "/" || next_token = "^" || next_token = "<" || 
       next_token = "<=" || next_token = ">" || next_token = ">=" || 
       next_token = "==" || next_token = "!=" then
      let T3 = expr_op T2
      expr_value T3
    else
      T2

  //
  // condition
  // grammar calls expression for comparison
  let rec private condition tokens =
    expr tokens

  //
  // output_value
  // console output grammar
  let rec private output_value tokens =
    let next_token = List.head tokens
    if next_token = "endl" then
      List.tail tokens
    else
      expr_value tokens

  //
  // empty
  // we expect an end of line semicolon
  let rec private empty tokens =
    eat ";" tokens

  //
  // vardecl
  // user wants to declar a variable
  let rec private vardecl tokens =
    let T2 = eat "int" tokens
    let T3 = eat "identifier:" T2
    eat ";" T3

  //
  // input
  // for cin operations
  let rec private input tokens =
    let T2 = eat "cin" tokens
    let T3 = eat ">>" T2
    let T4 = eat "identifier:" T3
    eat ";" T4

  //
  // output
  // for  cout operations
  let rec private output tokens =
    let T2 = eat "cout" tokens
    let T3 = eat "<<" T2
    let T4 = output_value T3
    eat ";" T4

  //
  // assignment
  // assign value to existing variable
  let rec private assignment tokens =
    let T2 = eat "identifier:" tokens
    let T3 = eat "=" T2
    let T4 = expr T3
    eat ";" T4

  //
  // stmt
  // parses one line of code
  let rec private stmt tokens =
    let next_token = List.head tokens
    match next_token with
    | ";" -> empty tokens
    | "int" -> vardecl tokens
    | "cin" -> input tokens
    | "cout" -> output tokens
    | "if" -> ifstmt tokens
    | _ when beginswith "identifier:" next_token -> assignment tokens
    | _ -> failwith ("expecting statement, but found " + next_token)

  //
  // then_part
  // parses to statements in an if condition
  and private then_part tokens =
    stmt tokens

  //
  // else_part
  // parses else code block in if statements
  and private else_part tokens =
    let next_token = List.head tokens
    if next_token = "else" then
      let T2 = eat "else" tokens
      stmt T2
    else
      tokens

  //
  // ifstmt
  // parses syntax for reading if statetment grammars
  and private ifstmt tokens =
    let T2 = eat "if" tokens
    let T3 = eat "(" T2
    let T4 = condition T3
    let T5 = eat ")" T4
    let T6 = then_part T5
    else_part T6

  //
  // morestmts
  // multiple statements in a block with end markers grammar
  let rec private morestmts tokens =
    let next_token = List.head tokens
    if next_token = "}" || next_token = "$" then
      tokens
    else
      let T2 = stmt tokens
      morestmts T2

  //
  // stmts
  // we verify atleast one line exists and calls block grammar
  let rec private stmts tokens =
    let T2 = stmt tokens
    morestmts T2

  //
  // simpleC
  // grammar for main program skeleton
  let private simpleC tokens = 
    let T2 = eat "void" tokens
    let T3 = eat "main" T2
    let T4 = eat "(" T3
    let T5 = eat ")" T4
    let T6 = eat "{" T5
    let T7 = stmts T6
    let T8 = eat "}" T7
    eat "$" T8

  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid SimpleC program.  Returns
  // the string "success" if valid, otherwise returns a 
  // string of the form "syntax_error:...".
  //
  let parse tokens = 
    try
      let result = simpleC tokens
      "Success!"
    with 
      | ex -> "syntax_error: " + ex.Message