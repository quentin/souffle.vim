" Vim syntax file
" Language: Souffle datalog engine
" Maintainer: Nic H
" Latest Revision: 22 November 2016

if exists("b:current_syntax")
  finish
endif

" Binary operator
syn match souffleBinaryOp "[-+*/%]" 
      \ contained
      \ nextgroup=@souffleArgument
hi def link souffleBinaryOp Operator

" Constraint operator
syn match souffleConstraintOp "<\|>\|=" contained nextgroup=@souffleArgument
syn match souffleConstraintOp "<=\|>=\|==\|!=" contained nextgroup=@souffleArgument
hi def link souffleConstraintOp Operator

" Comments
syn region  souffleBlockComment   start="/\*" end="\*/" contains=@souffleCommentContent
syn region  souffleInlineComment  start="//" end="$" contains=@souffleCommentContent
syn cluster souffleComment        contains=souffleBlockComment,souffleInlineComment
syn cluster souffleCommentContent contains=souffleTodo
syn keyword souffleTodo           TODO FIXME NOTE contained

" Relation declaration identifier
syn match   souffleRelationDeclIdentId "[a-zA-Z][_?a-zA-Z0-9]*\|[_?][_?a-zA-Z0-9]\+"
      \ contained skipwhite skipempty
      \ nextgroup=@souffleRelationDeclIdentSep,souffleRelationDeclAttributes
hi def link souffleRelationDeclIdentId PreProc
syn cluster souffleRelationDeclIdent contains=souffleRelationDeclIdentId
syn region  souffleRelationDeclIdentBlockComment  start="/\*" end="\*/"
      \ contains=@souffleCommentContent
      \ contained skipwhite skipempty
      \ nextgroup=@souffleRelationDeclIdent
syn region  souffleRelationDeclIdentInlineComment start="//" end="$"
      \ contains=@souffleCommentContent
      \ contained skipempty
      \ nextgroup=@souffleRelationDeclIdent
hi def link souffleRelationDeclIdentBlockComment  Comment
hi def link souffleRelationDeclIdentInlineComment Comment
syn cluster souffleRelationDeclIdent add=souffleRelationDeclIdentBlockComment,souffleRelationDeclIdentInlineComment

" Relation declaration identifier separator
syn match souffleRelationDeclIdentSepSign ","
      \ contained skipwhite skipempty
      \ nextgroup=@souffleRelationDeclIdent
syn cluster souffleRelationDeclIdentSep contains=souffleRelationDeclIdentSepSign
hi def link souffleRelationDeclIdentSepSign Statement
syn region  souffleRelationDeclIdentSepBlockComment  start="/\*" end="\*/"
      \ contains=@souffleCommentContent
      \ contained skipwhite skipempty
      \ nextgroup=@souffleRelationDeclIdentSep
syn region  souffleRelationDeclIdentSepInlineComment start="//" end="$"
      \ contains=@souffleCommentContent
      \ contained skipempty
      \ nextgroup=@souffleRelationDeclIdentSep
hi def link souffleRelationDeclIdentSepBlockComment  Comment
hi def link souffleRelationDeclIdentSepInlineComment Comment
syn cluster souffleRelationDeclIdentSep add=souffleRelationDeclIdentSepBlockComment,souffleRelationDeclIdentSepInlineComment

" Relation declaration attributes
syn region souffleRelationDeclAttributes matchgroup=PreProc start="(" end=")"
      \ contained skipwhite skipempty
      \ contains=@souffleRelationAttributeIdent
      \ nextgroup=souffleRelationQualifier,souffleRelationChoiceDomain

" Relation attribute identifier
syn match souffleRelationAttributeIdentId "[a-zA-Z][_?a-zA-Z0-9]*\|[_?][_?a-zA-Z0-9]\+"
      \ contained skipwhite skipempty
      \ nextgroup=@souffleRelationAttributeTypeSep
hi def link souffleRelationAttributeIdentId Identifier
syn cluster souffleRelationAttributeIdent contains=souffleRelationAttributeIdentId

" Relation attribute type separator
syn match souffleRelationAttributeTypeSepSign ":"
      \ contained skipwhite skipempty
      \ nextgroup=@souffleRelationAttributeTypeIdent
hi def link souffleRelationAttributeTypeSepSign Statement
syn cluster souffleRelationAttributeTypeSep contains=souffleRelationAttributeTypeSepSign

" Relation attribute type identifier
syn match souffleRelationAttributeTypeIdentId "[a-zA-Z][_?a-zA-Z0-9]*\|[_?][_?a-zA-Z0-9]\+"
      \ contained skipwhite skipempty
      \ nextgroup=@souffleRelationAttributeSep,@souffleRelationAttributeTypeQualifiedSep
hi def link souffleRelationAttributeTypeIdentId Type
syn cluster souffleRelationAttributeTypeIdent contains=souffleRelationAttributeTypeIdentId

" Relation attribute type qualified identifier separator
syn match souffleRelationAttributeTypeQualifiedSepSign "\."
      \ contained skipwhite skipempty
      \ nextgroup=@souffleRelationAttributeTypeIdent
hi def link souffleRelationAttributeTypeQualifiedSepSign Operator
syn cluster souffleRelationAttributeTypeQualifiedSep contains=souffleRelationAttributeTypeQualifiedSepSign

" Type declaration identifier
syn match   souffleTypeDeclIdentId "[a-zA-Z][_?a-zA-Z0-9]*\|[_?][_?a-zA-Z0-9]\+"
  \ contained skipwhite skipempty
  \ nextgroup=@souffleSubtypeDecl,@souffleEquivTypeDecl
hi def link souffleTypeDeclIdentId   Type
syn cluster souffleTypeDeclIdent
      \ contains=souffleTypeDeclIdentId
syn region  souffleTypeDeclIdentBlockComment  start="/\*" end="\*/"
      \ contains=@souffleCommentContent
      \ contained skipwhite skipempty
      \ nextgroup=@souffleTypeDeclIdent
syn region  souffleTypeDeclIdentInlineComment start="//" end="$"
      \ contains=@souffleCommentContent
      \ contained skipempty
      \ nextgroup=@souffleTypeDeclIdent
syn cluster souffleTypeDeclIdent
      \ add=souffleTypeDeclIdentBlockComment,souffleTypeDeclIdentInlineComment
hi def link souffleTypeDeclIdentBlockComment  Comment
hi def link souffleTypeDeclIdentInlineComment Comment

" Equivalent type declaration
syn match souffleEquivTypeDeclSign "="
      \ contained skipwhite skipempty
      \ nextgroup=@souffleTypeDef
syn region souffleEquivTypeDeclBlockComment
      \ start="/\*" end="\*/"
      \ nextgroup=@souffleEquivTypeDecl
      \ contains=@souffleCommentContent
      \ contained skipwhite skipempty
syn region souffleEquivTypeDeclInlineComment start="//" end="$"
      \ nextgroup=@souffleEquivTypeDecl
      \ contains=@souffleCommentContent
      \ contained skipempty
syn cluster souffleEquivTypeDecl
      \ contains=souffleEquivTypeDeclSign,souffleEquivTypeDeclBlockComment,souffleEquivTypeDeclInlineComment
hi def link souffleEquivTypeDeclBlockComment  Comment
hi def link souffleEquivTypeDeclInlineComment Comment

" Type definition
syn match souffleTypeId "[a-zA-Z][_?a-zA-Z0-9]*\|[_?][_?a-zA-Z0-9]\+"
      \ contained
hi def link souffleTypeId        Type
syn cluster souffleTypeDef
      \ contains=souffleTypeId
syn region  souffleTypeDefBlockComment
      \ start="/\*" end="\*/"
      \ nextgroup=@souffleTypeDef
      \ contains=@souffleCommentContent
      \ contained skipwhite skipempty
syn region  souffleTypeDefInlineComment start="//" end="$"
      \ nextgroup=@souffleTypeDef
      \ contains=@souffleCommentContent
      \ contained skipempty
syn cluster souffleTypeDef
      \ add=souffleTypeDefBlockComment,souffleTypeDefInlineComment,souffleTypeId
hi def link souffleTypeDefBlockComment   Comment
hi def link souffleTypeDeflInlineComment Comment

" Sub-Type definition
syn match souffleSubtypeDeclSign "<:"
      \ contained skipwhite skipempty
      \ nextgroup=souffleTypeName
hi def link souffleSubtypeDeclSign Operator
syn cluster souffleSubtypeDecl contains=souffleSubtypeDeclSign

" Type name
syn match souffleTypeName "[a-zA-Z][_?a-zA-Z0-9]*\|[_?][_?a-zA-Z0-9]\+"
      \ contained skipwhite skipempty
      \ nextgroup=@souffleStatements,souffleTypeQualifiedNameSep
hi def link souffleTypeName Type

" Type qualified name separator
syn match souffleTypeQualifiedNameSep "\."
      \ contained skipwhite skipempty
      \ nextgroup=souffleTypeName
hi def link souffleTypeQualifiedNameSep Operator

" Atom or Preprocessor macro
syn match souffleAtomIdent     "[a-zA-Z][_?a-zA-Z0-9]*\|[_?][_?a-zA-Z0-9]\+"
      \ skipwhite skipempty
      \ nextgroup=souffleAtomArguments,souffleAtomQualifiedIdentSep,@souffleStatements
hi def link souffleAtomIdent    Function
syn match souffleAtomQualifiedIdentSep "\."
      \ contained skipwhite skipempty
      \ nextgroup=souffleAtomIdent
hi def link souffleAtomQualifiedIdentSep Operator

" Atom arguments
syn region souffleAtomArguments contained
      \ matchgroup=PreProc
      \ start=/(/ end=/)/
      \ contains=@souffleArgument
      \ contained skipwhite skipempty
      \ nextgroup=@souffleStatements,souffleAtomSep,souffleRule,souffleFactSign

" Atom separator
syn match souffleAtomSep "," contained skipwhite skipempty nextgroup=souffleAtomIdent
hi def link souffleAtomSep PreProc

" Fact
syn match souffleFactSign "\." contained skipwhite skipempty
hi def link souffleFactSign Operator

" Rule
syn region souffleRule matchgroup=Statement start=":-" end="\."
      \ contained skipwhite skipempty
      \ contains=@souffleDisjunction nextgroup=@souffleQueryPlan

" Query plan
"TODO

" Disjunction
syn cluster souffleDisjunction contains=@souffleConjunction,souffleDisjunctionSep

" Disjunction separator
syn match souffleDisjunctionSep ";"
      \ contained skipwhite skipempty
      \ nextgroup=@souffleDisjunction
hi def link souffleDisjunctionSep Operator

" Conjunction
syn cluster souffleConjunction contains=@souffleConstraint,souffleBodyAtomDetection,souffleParenDisjunction,souffleConjunctionSep,souffleNegation

" Negation
syn match souffleNegation "!"
      \ contained skipwhite skipempty
      \ nextgroup=@souffleConstraint,souffleBodyAtomDetection,souffleParenDisjunction
hi def link souffleNegation Operator

" Conjunction separator
syn match souffleConjunctionSep ","
  \ contained skipwhite skipempty
  \ nextgroup=@souffleConjunction
hi def link souffleConjunctionSep Statement

" Parenthesized disjunction
syn region souffleParenDisjunction matchgroup=Statement start=/(/ end=/)/
      \ contained
      \ contains=@souffleDisjunction

" Constraint
syn cluster souffleConstraint contains=@souffleArgument,souffleMatch,souffleContains,souffleTrue,souffleFalse,@souffleComment,souffleConstraintOp

" Argument
syn cluster souffleArgument contains=@souffleConstant,souffleVariable,souffleNil,souffleLitRecord,souffleLitADT,souffleParenArgument,souffleAs,souffleUserdefFunctor,souffleIntrinsicFunctor,souffleAggregator,souffleUnaryOp,souffleBinaryOp

syn match souffleVariable "[a-zA-Z][_?a-zA-Z0-9]*\|[_?][_?a-zA-Z0-9]\+"
      \ contained
hi def link souffleVariable Identifier

" Binary operator
syn match souffleBinaryOp "\<\(land\|lor\|lxor\|band\|bor\|bxor\|bshl\|bshr\|bshru\)\>"
      \ contained skipwhite skipempty
      \ nextgroup=@souffleArgument

" Intrinsic functor
syn match souffleIntrinsicFunctor "\<\(ord\|to_float\|to_number\|to_string\|to_unsigned\|cat\|strlen\|substr\)\>"
      \ contained skipwhite skipempty
      \ nextgroup=souffleBodyAtomArguments
hi def link souffleIntrinsicFunctor Function

" User defined functor
syn match souffleUserdefFunctor "@\<[a-zA-Z][a-zA-Z0-9_]*\>"
      \ contained skipwhite skipempty
      \ nextgroup=souffleBodyAtomArguments
hi def link souffleUserdefFunctor Function

" Constants
syn cluster souffleConstant contains=souffleLitString,souffleLitNumber,souffleLitUnsigned,souffleLitFloat,souffleLitIPv4

" Literal string
syn region souffleLitString matchgroup=String start=/"/ end=/"/ skip=/\\"/
      \ contained skipwhite skipempty
      \ nextgroup=@souffleComment
hi def link souffleLitString String

" Literal number
syn match souffleLitNumber "\<[0-9]\+\>" contained
syn match souffleLitNumber "\<0b[01]\+\>" contained
syn match souffleLitNumber "\<0x[0-9a-fA-F]\+\>" contained
hi def link souffleLitNumber Number

" Literal float
syn match souffleLitFloat /[0-9]\+\.[0-9]\+/ contained
hi def link souffleLitFloat Float

" Literal IPv4 address
syn match souffleLitIPv4 "[0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+" contained
hi def link souffleLitIPv4 Number

" Literal unsigned
syn match souffleLitUnsigned "\<[0-9]\+u\>" contained
syn match souffleLitUnsigned "\<0b[01]\+u\>" contained
syn match souffleLitUnsigned "\<0x[0-9a-fA-F]\+u\>" contained
hi def link souffleLitUnsigned Number

" Body Atom detection
" Does not detect case where a comment is inserted between the identifier and
" the opening parenthesis
syn match souffleBodyAtomDetection "\([a-zA-Z][_?a-zA-Z0-9]*\|[_?][_?a-zA-Z0-9]\+\)\([ \t\n]*\.[ \t\n]*[a-zA-Z][_?a-zA-Z0-9]*\|[_?][_?a-zA-Z0-9]\+\)*[ \t\n]*("
      \ contained contains=souffleBodyAtomIdent,@souffleComment

" Body Atom identifier
syn match souffleBodyAtomIdent "\<[a-zA-Z_][a-zA-Z_0-9]*\>"
      \ contained skipwhite skipempty
      \ nextgroup=souffleBodyAtomArguments,souffleBodyAtomQualifiedIdentSep
hi def link souffleBodyAtomIdent    Identifier

" Body Atom identifier qualified name separator
syn match souffleBodyAtomQualifiedIdentSep "\."
      \ contained skipwhite skipempty
      \ nextgroup=souffleBodyAtomIdent
hi def link souffleBodyAtomQualifiedIdentSep Operator

" Body Atom arguments
syn region souffleBodyAtomArguments
      \ matchgroup=PreProc
      \ start="(" end=")"
      \ contained skipwhite skipempty
      \ contains=@souffleArgument,@souffleComment

" Nil literal value
syn match souffleNil "\<nil\>" contained
hi def link souffleNil Constant

" True constraint
syn match souffleTrue "\<true\>" contained
hi def link souffleTrue Boolean

" False constraint
syn match souffleFalse "\<false\>" contained
hi def link souffleFalse Boolean

" Directive name
syn match souffleDirectiveName  "[a-zA-Z][_?a-zA-Z0-9]*\|[_?][_?a-zA-Z0-9]\+"
      \ contained skipwhite skipempty
      \ nextgroup=souffleDirectiveQualifiedNameSep,souffleDirectiveNameSep,souffleDirectiveArglist
hi def link souffleDirectiveName PreProc

" Directive qualified name separator
syn match souffleDirectiveQualifiedNameSep "\."
      \ contained skipwhite skipempty
      \ nextgroup=souffleDirectiveName
hi def link souffleDirectiveQualifiedNameSep Operator

" Directive name separator
syn match souffleDriectiveNameSep ","
      \ contained skipwhite skipempty
      \ nextgroup=souffleDirectiveArglist

" Directive arguments list
syn region souffleDirectiveArglist start="(" end=")"
      \ contained
      \ contains=souffleDirectiveArgumentIdent

" Directive argument identifier
syn match souffleDirectiveArgumentIdent "[a-zA-Z][_?a-zA-Z0-9]*\|[_?][_?a-zA-Z0-9]\+"
      \ contained skipwhite skipempty
      \ nextgroup=souffleDirectiveArgumentEqual
hi def link souffleDirectiveArgumentIdent Identifier

" Directive argument equal sign
syn match souffleDirectiveArgumentEqual "="
      \ contained skipwhite skipempty
      \ nextgroup=@souffleDirectiveArgumentValue

" Directive argument value
syn cluster souffleDirectiveArgumentValue contains=souffleLitString,souffleLitNumber,souffleTrue,souffleFalse,souffleIdent

" Souffle identifier
syn match souffleIdent "[a-zA-Z][_?a-zA-Z0-9]*\|[_?][_?a-zA-Z0-9]\+"
      \ contained
hi def link souffleIdent Identifier

" Statements
syn match   souffleTypeKey       "\.type\>" skipwhite skipempty nextgroup=@souffleTypeDeclIdent
hi def link souffleTypeKey        Statement

syn match   souffleDeclKey       "\.decl\>" skipwhite skipempty nextgroup=@souffleRelationDeclIdent
hi def link souffleDeclKey        Statement

syn match   souffleInputKey      "\.input\>" skipwhite skipempty nextgroup=souffleDirectiveName
hi def link souffleInputKey      Statement

syn match   souffleOutputKey     "\.output\>" skipwhite skipempty nextgroup=souffleDirectiveName
hi def link souffleOutputKey     Statement

syn cluster souffleStatements contains=souffleTypeKey,souffleDeclKey,souffleInputKey,souffleOutputKey

let b:current_syntax = "souffle"

hi def link souffleBlockComment  Comment
hi def link souffleInlineComment Comment

hi def link souffleCompKey       Statement
hi def link souffleDefCSep       Special
hi def link souffleDefKey        Statement
hi def link soufflePragmaKey     Statement
hi def link souffleIOField       PreProc
hi def link soufflePreproc       PreProc
hi def link souffleRuleBodyEnd   Special
hi def link souffleRuleBodyStart Special
hi def link souffleTodo          Todo
hi def link souffleTypeKey       Statement
hi def link souffleRelId         Type
hi def link souffleVarId         Identifier
hi def link souffleConstantId    Constant
hi def link souffleOp            Special
hi def link soufflePlanKey       Statement
hi def link soufflePlanVer       Identifier
hi def link soufflePlanOrderId   Special
