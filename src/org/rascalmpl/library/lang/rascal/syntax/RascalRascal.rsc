@doc{The syntax definition of Rascal, excluding concrete syntax fragments}
@bootstrapParser
module lang::rascal::syntax::RascalRascal

syntax BooleanLiteral
	= lex "true" 
	| lex "false" ;

syntax Literal
	= Integer: IntegerLiteral integerLiteral 
	| RegExp: RegExpLiteral regExpLiteral 
	| Real: RealLiteral realLiteral 
	| Boolean: BooleanLiteral booleanLiteral 
	| String: StringLiteral stringLiteral 
	| DateTime: DateTimeLiteral dateTimeLiteral 
	| Location: LocationLiteral locationLiteral ;

start syntax Module
	= Default: Header header Body body ;

start syntax PreModule
    = Default: Header header Marker Rest;

syntax Marker = 
              # "import"
              # "syntax"
              # "start"
              # "layout"
              # "lexical"
              # "extend"
              ;

syntax Rest = Word*;

syntax Word = lex ![\ \t\n\r]+
            # ![\ \t\n\r]
            ;          
                   
syntax Alternative
	= NamedType: Name name Type type ;

syntax ModuleParameters
	= Default: "[" {TypeVar ","}+ parameters "]" ;

syntax DateAndTime
	= lex "$" DatePart "T" TimePartNoTZ TimeZonePart? ;

syntax Asterisk
	= lex [*] 
	# [/] ;

syntax Strategy
	= TopDownBreak: "top-down-break" 
	| TopDown: "top-down" 
	| BottomUp: "bottom-up" 
	| BottomUpBreak: "bottom-up-break" 
	| Outermost: "outermost" 
	| Innermost: "innermost" ;

syntax UnicodeEscape
	= lex "\\" [u]+ [0-9 A-F a-f] [0-9 A-F a-f] [0-9 A-F a-f] [0-9 A-F a-f] ;

syntax Variable
	= Initialized: Name name "=" Expression initial 
	| UnInitialized: Name name ;

syntax OctalIntegerLiteral
	= lex [0] [0-7]+ 
	# [0-9 A-Z _ a-z] ;

syntax TypeArg
	= Default: Type type 
	| Named: Type type Name name ;

syntax Renaming
	= Default: Name from "=\>" Name to ;

syntax Catch
	= Default: "catch" ":" Statement body 
	| Binding: "catch" Pattern pattern ":" Statement body ;

syntax PathChars
	= lex URLChars [|] ;

syntax Signature
	= WithThrows: Type type FunctionModifiers modifiers Name name Parameters parameters "throws" {Type ","}+ exceptions 
	| NoThrows: Type type FunctionModifiers modifiers Name name Parameters parameters ;

syntax HexLongLiteral
	= lex [0] [X x] [0-9 A-F a-f]+ [L l] 
	# [0-9 A-Z _ a-z] ;

syntax Sym
	= Nonterminal: Nonterminal nonterminal
	| Parameter: "&" Nonterminal nonterminal 
	| Parametrized: ParameterizedNonterminal pnonterminal "[" {Sym ","}+ parameters "]"
	| Keyword: "keyword" "[" Nonterminal nonterminal "]"
	| Start: "start" "[" Nonterminal nonterminal "]"
	| Layout: "layout" "[" Nonterminal nonterminal "]"
	| CharacterClass: Class charClass 
	| Literal: StringConstant string 
	| Labeled: Sym symbol NonterminalLabel label 
	| CaseInsensitiveLiteral: CaseInsensitiveStringConstant cistring
	| Iter: Sym symbol "+" 
	| IterStar: Sym symbol "*" 
	| IterSep: "{" Sym symbol StringConstant sep "}" "+" 
	| IterStarSep: "{" Sym symbol StringConstant sep "}" "*" 
	| Optional: Sym symbol "?" 
	| Alternative: "(" Sym first "|" {Sym "|"}+ alternatives ")"
	| Sequence: "(" Sym first Sym+ sequence ")"
	| Empty: "(" ")"
	| Column: "@" IntegerLiteral column
	| EndOfLine: "$"
	| StartOfLine: "^" 
	> non-assoc ( Follow:     Sym symbol "\>\>" Sym match
	            | NotFollow:  Sym symbol "!\>\>" Sym match
	            | Precede:    Sym match "\<\<" Sym symbol
	            | NotPrecede: Sym match "!\<\<" Sym symbol
	            | Unequal:    Sym symbol "!=" Sym match
	            )
	;

syntax TimePartNoTZ
	= lex [0-2] [0-9] [0-5] [0-9] [0-5] [0-9]
	| lex [0-2] [0-9] [0-5] [0-9] [0-5] [0-9] [, .] [0-9] 
	| lex [0-2] [0-9] [0-5] [0-9] [0-5] [0-9] [, .] [0-9] [0-9] 
	| lex [0-2] [0-9] [0-5] [0-9] [0-5] [0-9] [, .] [0-9] [0-9] [0-9] 
	| lex [0-2] [0-9] ":" [0-5] [0-9] ":" [0-5] [0-9] 
	| lex [0-2] [0-9] ":" [0-5] [0-9] ":" [0-5] [0-9] [, .] [0-9] 
	| lex [0-2] [0-9] ":" [0-5] [0-9] ":" [0-5] [0-9] [, .] [0-9] [0-9]
	| lex [0-2] [0-9] ":" [0-5] [0-9] ":" [0-5] [0-9] [, .] [0-9] [0-9] [0-9] 
	;

syntax DecimalLongLiteral
	= lex "0" [L l] 
	| lex [1-9] [0-9]* [L l] 
	# [0-9 A-Z _ a-z] ;

syntax Header
	= Parameters: Tags tags "module" QualifiedName name ModuleParameters params Import* imports 
	| Default: Tags tags "module" QualifiedName name Import* imports ;

syntax Name
	= lex [A-Z _ a-z] [0-9 A-Z _ a-z]* 
	| lex EscapedName 
	- RascalReservedKeywords 
	# [0-9 A-Z _ a-z] ;

syntax SyntaxDefinition
	= Layout  : "layout"  Sym defined "=" Prod production ";" 
	| Lexical : "lexical" Sym defined "=" Prod production ";" 
	| Keyword : "keyword" Sym defined "=" Prod production ";"
	| Language: Start start "syntax" Sym defined "=" Prod production ";" ;

syntax Kind
	= Function: "function" 
	| Variable: "variable" 
	| All: "all" 
	| Anno: "anno" 
	| Data: "data" 
	| View: "view" 
	| Rule: "rule" 
	| Alias: "alias" 
	| Module: "module" 
	| Tag: "tag" ;

syntax Test
	= Labeled: Tags tags "test" Expression expression ":" StringLiteral labeled 
	| Unlabeled: Tags tags "test" Expression expression ;

syntax ImportedModule
	= Default: QualifiedName name 
	| ActualsRenaming: QualifiedName name ModuleActuals actuals Renamings renamings 
	| Renamings: QualifiedName name Renamings renamings 
	| Actuals: QualifiedName name ModuleActuals actuals ;

syntax Target
	= Empty: 
	| Labeled: Name name ;

syntax IntegerLiteral
	= /*prefer()*/ DecimalIntegerLiteral: DecimalIntegerLiteral decimal 
	| /*prefer()*/ HexIntegerLiteral: HexIntegerLiteral hex 
	| /*prefer()*/ OctalIntegerLiteral: OctalIntegerLiteral octal ;

syntax FunctionBody
	= Default: "{" Statement* statements "}" ;
    
syntax Expression
	= NonEmptyBlock  : "{" Statement+ statements "}" 
	| bracket Bracket: "(" Expression expression ")" 
	| Closure        : Type type Parameters parameters "{" Statement+ statements "}" 
	| StepRange      : "[" Expression first "," Expression second ".." Expression last "]" 
	| VoidClosure    : Parameters parameters "{" Statement* statements "}" 
	| Visit          : Label label Visit visit 
	| Reducer        : "(" Expression init "|" Expression result "|" {Expression ","}+ generators ")" 
	| ReifiedType    : BasicType basicType "(" {Expression ","}* arguments ")" 
	| CallOrTree     : Expression expression "(" {Expression ","}* arguments ")"
	| Literal        : Literal literal 
	| Any            : "any" "(" {Expression ","}+ generators ")" 
	| All            : "all" "(" {Expression ","}+ generators ")" 
	| Comprehension  : Comprehension comprehension 
	| Set            : "{" {Expression ","}* elements "}" 
	| List           : "[" {Expression ","}* elements "]"
	| ReifyType      : "#" Type type 
	| Range          : "[" Expression first ".." Expression last "]"
	| Tuple          : "\<" {Expression ","}+ elements "\>" 
	| Map            : "(" {Mapping[Expression] ","}* mappings ")" 
	| It             : "it" 
	| QualifiedName  : QualifiedName qualifiedName 
	
	>  Subscript  : Expression expression "[" {Expression ","}+ subscripts "]" 
	| FieldAccess : Expression expression "." Name field 
	| FieldUpdate : Expression expression "[" Name key "=" Expression replacement "]" 
	| FieldProject: Expression expression "\<" {Field ","}+ fields "\>" 
	| Is : Expression expression "is" Name name
	| Has : Expression expression "has" Name name
	> IsDefined: Expression argument "?" 
	> Negation: "!" Expression argument 
	| Negative: "-" Expression argument 
	> TransitiveClosure: Expression argument "+" 
	| TransitiveReflexiveClosure: Expression argument "*" 
	> SetAnnotation: Expression expression "[" "@" Name name "=" Expression value "]" 
	| GetAnnotation: Expression expression "@" Name name 
	> left Composition: Expression lhs "o" Expression rhs 
	> left ( Product: Expression lhs "*" Expression rhs  
		   | Join   : Expression lhs "join" Expression rhs 
	       )
	> left ( Modulo: Expression lhs "%" Expression rhs  
		   | Division: Expression lhs "/" Expression rhs 
	       )
	> left Intersection: Expression lhs "&" Expression rhs 
	> left ( Addition   : Expression lhs "+" Expression rhs  
		   | Subtraction: Expression lhs "-" Expression rhs 
	       )
	> non-assoc (  NotIn: Expression lhs "notin" Expression rhs  
		        |  In: Expression lhs "in" Expression rhs 
	)
	> non-assoc ( GreaterThanOrEq: Expression lhs "\>=" Expression rhs  
		        | LessThanOrEq   : Expression lhs "\<=" Expression rhs 
		        | LessThan       : Expression lhs "\<" Expression rhs 
		        | GreaterThan    : Expression lhs "\>" Expression rhs 
	            )
	> left IfThenElse: Expression condition "?" Expression thenExp ":" Expression elseExp
	> non-assoc ( Equals         : Expression lhs "==" Expression rhs
	            | NonEquals      : Expression lhs "!=" Expression rhs 
	            )
	> non-assoc IfDefinedOtherwise: Expression lhs "?" Expression rhs
	> non-assoc ( NoMatch: Pattern pattern "!:=" Expression expression  
		        | Match: Pattern pattern ":=" Expression expression 
		        | /*prefer()*/ Enumerator: Pattern pattern "\<-" Expression expression 
	            ) 
	> non-assoc ( Implication: Expression lhs "==\>" Expression rhs  
		        | Equivalence: Expression lhs "\<==\>" Expression rhs 
	            )
	> left And: Expression lhs "&&" Expression rhs 
	> left Or: Expression lhs "||" Expression rhs 
	; 

syntax UserType
	= Name: QualifiedName name 
	| Parametric: QualifiedName name "[" {Type ","}+ parameters "]" ;

syntax Import
	= Extend: "extend" ImportedModule module ";" 
	| Default: "import" ImportedModule module ";" 
	| Syntax: SyntaxDefinition syntax ;

syntax Body
	= Toplevels: Toplevel* toplevels ;

syntax URLChars
	= lex ![\t-\n \r \  \< |]* ;

syntax LanguageAction
	= Replace: "=\>" Expression expression ";"
	| When: "when" {Expression ","}+ conditions ";"
	| ReplaceWhen: "=\>" Expression expression "when" {Expression ","}+ conditions ";" 
	| Action: "{" Statement* statements "}" ;

syntax TimeZonePart
	= lex [+ \-] [0-1] [0-9] ":" [0-5] [0-9] 
	| lex "Z" 
	| lex [+ \-] [0-1] [0-9] 
	| lex [+ \-] [0-1] [0-9] [0-5] [0-9] ;

syntax ProtocolPart
	= NonInterpolated: ProtocolChars protocolChars 
	| Interpolated: PreProtocolChars pre Expression expression ProtocolTail tail ;

syntax StringTemplate
	= IfThen    : "if"    "(" {Expression ","}+ conditions ")" "{" Statement* preStats StringMiddle body Statement* postStats "}" 
	| IfThenElse: "if"    "(" {Expression ","}+ conditions ")" "{" Statement* preStatsThen StringMiddle thenString Statement* postStatsThen "}" "else" "{" Statement* preStatsElse StringMiddle elseString Statement* postStatsElse "}" 
	| For       : "for"   "(" {Expression ","}+ generators ")" "{" Statement* preStats StringMiddle body Statement* postStats "}" 
	| DoWhile   : "do"    "{" Statement* preStats StringMiddle body Statement* postStats "}" "while" "(" Expression condition ")" 
	| While     : "while" "(" Expression condition ")" "{" Statement* preStats StringMiddle body Statement* postStats "}" ;

syntax PreStringChars
	= @category="Constant" lex [\"] StringCharacter* [\<] ;

syntax CaseInsensitiveStringConstant
	= @category="Constant" lex "\'" StringCharacter* "\'" ;

syntax Backslash
	= lex [\\] 
	# [/ \< \> \\] ;

syntax Label
	= Default: Name name ":" 
	| Empty: ;

syntax ShortChar
	= lex [0-9 A-Z a-z] character 
	| lex [\\] [\032-/ :-@ \[-` n r t {-\uFFFF] escape ;

syntax NumChar
	= lex [\\] [0-9]+ number ;

syntax NoElseMayFollow
	= Default: 
	# "else" ;

syntax MidProtocolChars
	= lex "\>" URLChars "\<" ;

syntax NamedBackslash
	= lex [\\] 
	# [\< \> \\] ;

syntax Field
	= Index: IntegerLiteral fieldIndex 
	| Name: Name fieldName ;

syntax JustDate
	= lex "$" DatePart ;

syntax PostPathChars
	= lex "\>" URLChars "|" ;

syntax PathPart
	= NonInterpolated: PathChars pathChars 
	| Interpolated: PrePathChars pre Expression expression PathTail tail ;

syntax DatePart
	= lex [0-9] [0-9] [0-9] [0-9] "-" [0-1] [0-9] "-" [0-3] [0-9] 
	| lex [0-9] [0-9] [0-9] [0-9] [0-1] [0-9] [0-3] [0-9] ;

syntax FunctionModifier
	= Java: "java" 
	| Default: "default";

syntax Assignment
	= IfDefined: "?=" 
	| Division: "/=" 
	| Product: "*=" 
	| Intersection: "&=" 
	| Subtraction: "-=" 
	| Default: "=" 
	| Addition: "+=" ;

syntax Assignable
	= bracket Bracket   : "(" Assignable arg ")"
	| Variable          : QualifiedName qualifiedName
    | Subscript         : Assignable receiver "[" Expression subscript "]" 
	| FieldAccess       : Assignable receiver "." Name field 
	| IfDefinedOrDefault: Assignable receiver "?" Expression defaultExpression 
	| Constructor       : Name name "(" {Assignable ","}+ arguments ")"  
	| Tuple             : "\<" {Assignable ","}+ elements "\>" 
	| Annotation        : Assignable receiver "@" Name annotation  ;

syntax StringConstant
	= @category="Constant" lex "\"" StringCharacter* "\"" ;

syntax Assoc
	= Associative: "assoc" 
	| Left: "left" 
	| NonAssociative: "non-assoc" 
	| Right: "right" ;

syntax Replacement
	= Unconditional: Expression replacementExpression 
	| Conditional: Expression replacementExpression "when" {Expression ","}+ conditions ;

syntax ParameterizedNonterminal
	= lex [A-Z] [0-9 A-Z _ a-z]* 
	- "start"
	- "keyword"
	- "layout"
	# ![\[] ;

syntax TagChar
	= lex ![}] 
	| lex [\\] [\\ }] ;

syntax DataTarget
	= Empty: 
	| Labeled: Name label ":" ;

syntax StringCharacter
	= lex "\\" [\" \' \< \> \\ b f n r t] 
	| lex UnicodeEscape 
	| lex OctalEscapeSequence 
	| lex ![\" \' \< \> \\]
	| lex [\n][\ \t]* [\'] // margin 
	;

syntax JustTime
	= lex "$T" TimePartNoTZ TimeZonePart? ;

syntax MidStringChars
	= @category="Constant" lex [\>] StringCharacter* [\<] ;

syntax ProtocolChars
	= lex [|] URLChars "://" ;

syntax RegExpModifier
	= lex [d i m s]* ;

syntax EscapedName
	= lex [\\] [A-Z _ a-z] [\- 0-9 A-Z _ a-z]* 
	# [\- 0-9 A-Z _ a-z] ;

syntax Parameters
	= Default: "(" Formals formals ")" 
	| VarArgs: "(" Formals formals "..." ")" ;

syntax RegExp
	= lex ![/ \< \> \\] 
	| lex "\<" Name "\>" 
	| lex [\\] [/ \< \> \\] 
	| lex "\<" Name ":" NamedRegExp* "\>" 
	| lex Backslash ;

layout LAYOUTLIST
	= LAYOUT* 
	# [\t-\n \r \ ] 
	# "//" 
	# "/*" ;

syntax LocalVariableDeclaration
	= Default: Declarator declarator 
	| Dynamic: "dynamic" Declarator declarator ;

syntax RealLiteral
	= lex [0-9]+ [D F d f] 
	| lex [0-9]+ [E e] [+ \-]? [0-9]+ [D F d f]?
	| lex [0-9]+ "." [0-9]* [D F d f]? 
	| lex [0-9]+ "." [0-9]* [E e] [+ \-]? [0-9]+ [D F d f]? 
	| lex "." [0-9]+ [D F d f]? 
	| lex "." [0-9]+ [E e] [+ \-]? [0-9]+ [D F d f]? 
	# [0-9 A-Z _ a-z] ;

syntax Range
	= FromTo: Char start "-" Char end 
	| Character: Char character ;

syntax LocationLiteral
	= Default: ProtocolPart protocolPart PathPart pathPart ;

syntax ShellCommand
	= SetOption: "set" QualifiedName name Expression expression 
	| Undeclare: "undeclare" QualifiedName name 
	| Help: "help" 
	| Edit: "edit" QualifiedName name 
	| Unimport: "unimport" QualifiedName name 
	| ListDeclarations: "declarations" 
	| Quit: "quit" 
	| History: "history" 
	| Test: "test" 
	| ListModules: "modules" ;

syntax StringMiddle
	= Mid: MidStringChars mid 
	| Template: MidStringChars mid StringTemplate template StringMiddle tail 
	| Interpolated: MidStringChars mid Expression expression StringMiddle tail ;

syntax QualifiedName
	= Default: {Name "::"}+ names 
	# "::" ;

syntax DecimalIntegerLiteral
	= lex "0" 
	| lex [1-9] [0-9]* 
	# [0-9 A-Z _ a-z] ;

syntax DataTypeSelector
	= Selector: QualifiedName sort "." Name production ;

syntax StringTail
	= MidInterpolated: MidStringChars mid Expression expression StringTail tail 
	| Post: PostStringChars post 
	| MidTemplate: MidStringChars mid StringTemplate template StringTail tail ;

syntax PatternWithAction
	= Replacing: Pattern pattern "=\>" Replacement replacement 
	| Arbitrary: Pattern pattern ":" Statement statement ;

syntax LAYOUT
	= lex Comment 
	| lex [\t-\n \r \ ] ;

syntax Visit
	= GivenStrategy: Strategy strategy "visit" "(" Expression subject ")" "{" Case+ cases "}" 
	| DefaultStrategy: "visit" "(" Expression subject ")" "{" Case+ cases "}" ;

start syntax Command
	= Expression: Expression expression {
	  if (expression is NonEmptyBlock) { 
	    fail;
	  }
	}
	| Declaration: Declaration declaration 
	| Shell: ":" ShellCommand command 
	| Statement: Statement statement {
	  // local variable declarations would be ambiguous with the "global" declarations defined above
	  if (statement is VariableDeclaration || statement is FunctionDeclaration || statement is Visit) { 
	    fail;
	  }
	}
	| Import: Import imported ;

syntax TagString
	= lex "{" TagChar* "}" ;

syntax ProtocolTail
	= Mid: MidProtocolChars mid Expression expression ProtocolTail tail 
	| Post: PostProtocolChars post ;

syntax Nonterminal
	= lex [A-Z] [0-9 A-Z _ a-z]* 
	# [0-9 A-Z _ a-z] 
	# [\[] ;

syntax PathTail
	= Mid: MidPathChars mid Expression expression PathTail tail 
	| Post: PostPathChars post ;

syntax CommentChar
	= lex Asterisk 
	| lex ![*] ;

syntax Visibility
	= Private: "private" 
	| Default: 
	| Public: "public" ;

syntax StringLiteral
	= Template: PreStringChars pre StringTemplate template StringTail tail 
	| Interpolated: PreStringChars pre Expression expression StringTail tail 
	| NonInterpolated: StringConstant constant ;

syntax Comment
	= @category="Comment" lex "/*" CommentChar* "*/" 
	| @category="Comment" lex "//" ![\n]* [\n] ;

syntax RegExp
	= @category="MetaVariable" lex [\<]  Expression  [\>] ;

syntax Renamings
	= Default: "renaming" {Renaming ","}+ renamings ;

syntax Tags
	= Default: Tag* tags ;

syntax Formals
	= Default: {Pattern ","}* formals ;

syntax PostProtocolChars
	= lex "\>" URLChars "://" ;

syntax Start
	= Absent: 
	| Present: "start" ;

syntax Statement
	= Assert: "assert" Expression expression ";" 
	| AssertWithMessage: "assert" Expression expression ":" Expression message ";" 
	| Expression: Expression expression ";" {
	   if (expression is NonEmptyBlock || expression is Visit) { 
	    fail;
	  }
	}
	| Visit: Label label Visit visit 
	| While: Label label "while" "(" {Expression ","}+ conditions ")" Statement body 
	| DoWhile: Label label "do" Statement body "while" "(" Expression condition ")" ";" 
	| For: Label label "for" "(" {Expression ","}+ generators ")" Statement body 
	| IfThen: Label label "if" "(" {Expression ","}+ conditions ")" Statement thenStatement NoElseMayFollow noElseMayFollow 
	| IfThenElse: Label label "if" "(" {Expression ","}+ conditions ")" Statement thenStatement "else" Statement elseStatement 
	| Switch: Label label "switch" "(" Expression expression ")" "{" Case+ cases "}" 
	| Fail: "fail" Target target ";" 
	| Break: "break" Target target ";" 
	| Continue: "continue" Target target ";" 
	| Solve: "solve" "(" {QualifiedName ","}+ variables Bound bound ")" Statement body 
	| non-assoc Try: "try" Statement body Catch+ handlers 
	| TryFinally: "try" Statement body Catch+ handlers "finally" Statement finallyBody 
	| NonEmptyBlock: Label label "{" Statement+ statements "}" 
	| EmptyStatement: ";" 
	| GlobalDirective: "global" Type type {QualifiedName ","}+ names ";" 
	| Assignment: Assignable assignable Assignment operator Statement statement
	| non-assoc ( Return    : "return" Statement statement  
		        | Throw     : "throw" Statement statement 
		        | Insert    : "insert" DataTarget dataTarget Statement statement 
		        | Append    : "append" DataTarget dataTarget Statement statement 
	            )
    > FunctionDeclaration: FunctionDeclaration functionDeclaration 
	| VariableDeclaration: LocalVariableDeclaration declaration ";"
	; 
	
    
syntax StructuredType
	= Default: BasicType basicType "[" {TypeArg ","}+ arguments "]" ;

syntax NonterminalLabel
	= lex [a-z] [0-9 A-Z _ a-z]* 
	# [0-9 A-Z _ a-z] ;

syntax FunctionType
	= TypeArguments: Type type "(" {TypeArg ","}* arguments ")" ;

syntax Case
	= PatternWithAction: "case" PatternWithAction patternWithAction 
	| Default: "default" ":" Statement statement ;

syntax Declarator
	= Default: Type type {Variable ","}+ variables ;

syntax Bound
	= Default: ";" Expression expression 
	| Empty: ;

syntax RascalReservedKeywords
	= "int" 
	| "true" 
	| "bag" 
	| "num" 
	| "node" 
	| "finally" 
	| "private" 
	| "real" 
	| "list" 
	| "fail" 
	| "lex" 
	| "if" 
	| "tag" 
	| "extend" 
	| "append" 
	| "repeat" 
	| "rel" 
	| "void" 
	| "non-assoc" 
	| "assoc" 
	| "test" 
	| "anno" 
	| "layout" 
	| "LAYOUT" 
	| "data" 
	| "join" 
	| "it" 
	| "bracket" 
	| "in" 
	| "import" 
	| "view" 
	| "false" 
	| "global" 
	| "all" 
	| "dynamic" 
	| "solve" 
	| "type" 
	| "try" 
	| "catch" 
	| "notin" 
	| "reified" 
	| "else" 
	| "insert" 
	| "switch" 
	| "return" 
	| "case" 
	| "adt" 
	| "while" 
	| "str" 
	| "throws" 
	| "visit" 
	| "tuple" 
	| "for" 
	| "assert" 
	| "loc" 
	| "default" 
	| "on" 
	| "map" 
	| "alias" 
	| "any" 
	| "module" 
	| "bool" 
	| "public" 
	| "one" 
	| "throw" 
	| "set" 
	| "cf" 
	| "fun" 
	| "non-terminal" 
	| "rule" 
	| "constructor" 
	| "datetime" 
	| "value" 
	# [\- 0-9 A-Z _ a-z] ;

syntax Type
	= bracket Bracket: "(" Type type ")" 
	| User: UserType user 
	| Function: FunctionType function 
	| Structured: StructuredType structured 
	| Basic: BasicType basic 
	| Selector: DataTypeSelector selector 
	| Variable: TypeVar typeVar 
	| Symbol: Sym symbol {
	   if (symbol is Nonterminal
	     ||symbol is Labeled
	     ||symbol is Parametrized
	     ||symbol is Parameter) {
	    fail;
	   }
	} 
	;

syntax Declaration
	= Variable    : Tags tags Visibility visibility Type type {Variable ","}+ variables ";" 
	| Annotation  : Tags tags Visibility visibility "anno" Type annoType Type onType "@" Name name ";" 
	| View        : Tags tags Visibility visibility "view" Name view "\<:" Name superType "=" {Alternative "|"}+ alts ";" 
	| Alias       : Tags tags Visibility visibility "alias" UserType user "=" Type base ";" 
	| Tag         : Tags tags Visibility visibility "tag" Kind kind Name name "on" {Type ","}+ types ";" 
	| DataAbstract: Tags tags Visibility visibility "data" UserType user ";" 
	| Data        : Tags tags Visibility visibility "data" UserType user "=" {Variant "|"}+ variants ";"
	| Rule        : Tags tags "rule" Name name PatternWithAction patternAction ";" 
	| Function    : FunctionDeclaration functionDeclaration 
	| Test        : Test test ";" 
	;

syntax Class
	= SimpleCharclass: "[" Range* ranges "]" 
	| Complement: "!" Class charClass 
	> left Difference: Class lhs "-" Class rhs 
	> left Intersection: Class lhs "&&" Class rhs 
	> left Union: Class lhs "||" Class rhs 
	| bracket Bracket: "(" Class charclass ")" ;

syntax RegExpLiteral
	= lex "/" RegExp* "/" RegExpModifier ;

syntax FunctionModifiers
	= List: FunctionModifier* modifiers ;

syntax Comprehension
	= Set: "{" {Expression ","}+ results "|" {Expression ","}+ generators "}" 
	| Map: "(" Expression from ":" Expression to "|" {Expression ","}+ generators ")" 
	| List: "[" {Expression ","}+ results "|" {Expression ","}+ generators "]" ;

syntax Variant
	= NAryConstructor: Name name "(" {TypeArg ","}* arguments ")" ;

syntax FunctionDeclaration
	= Abstract: Tags tags Visibility visibility Signature signature ";" 
	| Expression: Tags tags Visibility visibility Signature signature "=" Expression expression ";"
	| Default: Tags tags Visibility visibility Signature signature FunctionBody body ;

syntax PreProtocolChars
	= lex "|" URLChars "\<" ;

syntax NamedRegExp
	= lex "\<" Name "\>" 
	| lex [\\] [/ \< \> \\] 
	| lex NamedBackslash 
	| lex ![/ \< \> \\] ;

syntax ProdModifier
	= Associativity: Assoc associativity 
	| Bracket: "bracket" 
	| Lexical: "lex" 
	| Tag: Tag tag;

syntax Toplevel
	= GivenVisibility: Declaration declaration ;

syntax PostStringChars
	= @category="Constant" lex [\>] StringCharacter* [\"] ;

syntax HexIntegerLiteral
	= lex [0] [X x] [0-9 A-F a-f]+ 
	# [0-9 A-Z _ a-z] ;

syntax TypeVar
	= Free: "&" Name name 
	| Bounded: "&" Name name "\<:" Type bound ;

syntax OctalEscapeSequence
	= lex "\\" [0-7] [0-7] 
	| lex "\\" [0-3] [0-7] [0-7] 
	| lex "\\" [0-7] 
	# [0-7] ;

syntax OctalLongLiteral
	= lex [0] [0-7]+ [L l] 
	# [0-9 A-Z _ a-z] ;

syntax BasicType
	= Value: "value" 
	| Loc: "loc" 
	| Node: "node" 
	| Num: "num" 
	| ReifiedType: "type" 
	| Bag: "bag" 
	| Int: "int" 
	| Relation: "rel" 
	| ReifiedTypeParameter: "parameter" 
	| Real: "real" 
	| ReifiedFunction: "fun" 
	| Tuple: "tuple" 
	| String: "str" 
	| Bool: "bool" 
	| ReifiedReifiedType: "reified" 
	| Void: "void" 
	| ReifiedNonTerminal: "non-terminal" 
	| DateTime: "datetime" 
	| Set: "set" 
	| Map: "map" 
	| ReifiedConstructor: "constructor" 
	| List: "list" 
	| ReifiedAdt: "adt" 
	| Lex: "lex" ;

syntax Char
	= @category="Constant" lex "\\" [\  \" \' \- \< \> \[ \\ \] b f n r t] 
	| @category="Constant" lex ![\  \" \' \- \< \> \[ \\ \]] 
	| @category="Constant" lex UnicodeEscape 
	| @category="Constant" lex OctalEscapeSequence ;

syntax Prod
	= Reference: ":" Name referenced 
	| non-assoc Action: Prod prod LanguageAction action 
	| Labeled: ProdModifier* modifiers Name name ":" Sym* args 
	| Others: "..." 
	| Unlabeled: ProdModifier* modifiers Sym* args
	| AssociativityGroup: Assoc associativity "(" Prod group ")" 
	> left Reject: Prod lhs "-" Prod rhs 
	> left Follow: Prod lhs "#" Prod rhs 
	> left All   : Prod lhs "|" Prod rhs 
	> left First : Prod lhs "\>" Prod rhs
	;

syntax DateTimeLiteral
	= /*prefer()*/ DateLiteral: JustDate date 
	| /*prefer()*/ TimeLiteral: JustTime time 
	| /*prefer()*/ DateAndTimeLiteral: DateAndTime dateAndTime ;

syntax PrePathChars
	= lex URLChars "\<" ;

syntax Mapping[&T]
	= Default: &T from ":" &T to {
	  if (from is IfDefinedOtherwise) {
	    fail;
	  }
	} 
	;

syntax LongLiteral
	= /*prefer()*/ OctalLongLiteral: OctalLongLiteral octalLong 
	| /*prefer()*/ HexLongLiteral: HexLongLiteral hexLong 
	| /*prefer()*/ DecimalLongLiteral: DecimalLongLiteral decimalLong ;

syntax MidPathChars
	= lex "\>" URLChars "\<" ;

syntax Pattern
	= Set                 : "{" {Pattern ","}* elements "}" 
	| List                : "[" {Pattern ","}* elements "]" 
	| QualifiedName       : QualifiedName qualifiedName 
	| MultiVariable       : QualifiedName qualifiedName "*" 
	| Literal             : Literal literal 
	| Tuple               : "\<" {Pattern ","}+ elements "\>" 
	| TypedVariable       : Type type Name name 
	| Map                 : "(" {Mapping[Pattern] ","}* mappings ")" 
	| ReifiedType         : BasicType basicType "(" {Pattern ","}* arguments ")" 
	| CallOrTree          : Pattern expression "(" {Pattern ","}* arguments ")" 
	> VariableBecomes     : Name name ":" Pattern pattern
	| Guarded             : "[" Type type "]" Pattern pattern 
	| Descendant          : "/" Pattern pattern 
	| Anti                : "!" Pattern pattern 
	| TypedVariableBecomes: Type type Name name ":" Pattern pattern 
    ;
    
syntax Tag
	= @category="Comment" Default   : "@" Name name TagString contents 
	| @category="Comment" Empty     : "@" Name name 
	| @category="Comment" Expression: "@" Name name "=" Expression expression ;

syntax ModuleActuals
	= Default: "[" {Type ","}+ types "]" ;

syntax "map"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "loc"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "default"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "assert"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "visit"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "throws"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "value"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "datetime"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "fun"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "|"
	= ...
	# [|] ;

syntax "non-terminal"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "set"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "one"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "public"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "o"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "module"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "extend"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "append"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "fail"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "if"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "node"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "case"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "return"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "str"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "while"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "switch"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax ":"
	= ...
	# [:] ;

syntax ":"
	= ...
	# [=] ;

syntax "type"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "reified"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "notin"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "else"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "\>"
	= ...
	# [=] ;

syntax "?"
	= ...
	# [=] ;

syntax "dynamic"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "solve"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "\<"
	= ...
	# [=] ;

syntax "\<"
	= ...
	# [\-] ;

syntax "="
	= ...
	# [\>] ;

syntax "="
	= ...
	# [=] ;

syntax "false"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "on"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "!"
	= ...
	# [:] ;

syntax "!"
	= ...
	# [~] ;

syntax "!"
	= ...
	# [=] ;

syntax "&"
	= ...
	# [&] ;

syntax "&"
	= ...
	# [=] ;

syntax "in"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "*"
	= ...
	# [=] ;

syntax "join"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "+"
	= ...
	# [=] ;

syntax "it"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "."
	= ...
	# [.] ;

syntax "/"
	= ...
	# [=] ;

syntax ","
	= ...
	# ".." ;

syntax "-"
	= ...
	# [=] ;

syntax "tuple"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "for"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "constructor"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "rule"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "throw"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "bool"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "int"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "any"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "test"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "void"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "tag"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "://"
	= ...
	# [\t-\n \r \ ] ;

syntax "repeat"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "rel"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "real"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "list"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "finally"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "private"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "\<="
	= ...
	# "=\>";

syntax "num"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "true"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "adt"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "try"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "catch"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "insert"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "global"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "=="
	= ...
	# [\>] ;

syntax "all"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "view"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "import"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "data"
	= ...
	# [\- 0-9 A-Z _ a-z] ;

syntax "anno"
	= ...
	# [\- 0-9 A-Z _ a-z] ;