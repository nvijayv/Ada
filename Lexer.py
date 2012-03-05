import ply.lex as lex
from ply.lex import TOKEN
import re

reserved = ('ABORT', 'ABS', 'ABSTRACT', 'ACCEPT', 'ALIASED', 'ALL', 'AND', 'ARRAY', 'AT', 'BEGIN', 'BODY', 'CASE', 'CONSTANT', 'DECLARE', 'DELAY', 'DO', 'ELSE', 'ELSIF', 'END', 'ENTRY', 'EXCEPTION', 'EXIT', 'FOR', 'FUNCTION', 'GENERIC', 'GOTO', 'IF', 'IN', 'INTERFACE', 'IS', 'LIMITED', 'LOOP', 'NEW', 'NOT', 'NULL', 'OF', 'OR', 'OTHERS', 'OUT', 'OVERRIDING', 'PACKAGE', 'PRAGMA', 'PRIVATE', 'PROCEDURE', 'PROTECTED', 'RAISE', 'RECORD', 'REM', 'RENAMES', 'REQUEUE', 'RETURN', 'REVERSE', 'SELECT', 'SEPARATE', 'SUBTYPE', 'SYNCHRONIZED', 'TAGGED', 'TASK', 'TERMINATE', 'THEN', 'TYPE', 'UNTIL', 'USE', 'WHEN', 'WHILE', 'WITH', 'XOR')

attributes = ("ACCESS", "ADDRESS", "ADJACENT", "AFT", "ALIGNMENT", "BASE", "BIT_ORDER", "BODY_VERSION", "CALLABLE", "CALLER", "CEILING", "CLASS", "COMPONENT_SIZE", "COMPOSE", "CONSTRAINED", "COPY_SIGN", "COUNT", "DEFINITE", "DELTA", "DENORM", "DIGITS", "EXPONENT", "EXTERNAL_TAG", "FIRST", "FIRST_BIT", "FLOOR", "FORE", "FRACTION", "IDENTITY", "IMAGE", "INPUT", "LAST", "LAST_BIT", "LEADING_PART", "LENGTH", "MACHINE", "MACHINE_EMAX", "MACHINE_EMIN", "MACHINE_MANTISSA", "MACHINE_OVERFLOWS", "MACHINE_RADIX", "MACHINE_ROUNDING", "MACHINE_ROUNDS", "MAX", "MAX_SIZE_IN_STORAGE_ELEMENTS", "MIN", "MOD", "MODEL", "MODEL_EMIN", "MODEL_EPSILON", "MODEL_MANTISSA", "MODEL_SMALL", "MODULUS", "OUTPUT", "PARTITION_ID", "POS", "POSITION", "PRED", "PRIORITY", "RANGE", "READ", "REMAINDER", "ROUND", "ROUNDING", "SAFE_FIRST", "SAFE_LAST", "SCALE", "SCALING", "SIGNED_ZEROS", "SIZE", "SMALL", "STORAGE_POOL", "STORAGE_SIZE", "STREAM_SIZE", "SUCC", "TAG", "TERMINATED", "TRUNCATION", "UNBIASED_ROUNDING", "UNCHECKED_ACCESS", "VAL", "VALID", "VALUE", "VERSION", "WIDE_IMAGE", "WIDE_VALUE", "WIDE_WIDE_IMAGE", "WIDE_WIDE_VALUE", "WIDE_WIDE_WIDTH", "WIDE_WIDTH", "WIDTH", "WRITE")

rwords = {}
for i in reserved:
	rwords[i.lower()] = i

att = {}
for j in attributes:
	att['\''+j.lower()] = j
	
tokens = reserved + attributes + ('IDENTIFIER', 'ATTRIBUTE', 'TYPEID', 'INTEGER', 'FLOAT', 'CHAR', 'STRING', 'DELIMITER', 'PLUS', 'MINUS','TIMES','DIVIDE', 'LT', 'GT', 'LPAREN', 'RPAREN', 'LBRACKET', 'RBRACKET', 'LBRACE', 'RBRACE', 'QUOT', 'HASH', 'TICK', 'COMMA', 'PERIOD', 'SEMIC', 'COLON', 'ARROW', 'DDOT', 'DSTAR', 'ASSIGN', 'EQUALS', 'NEQUALS', 'GE', 'LE', 'LSHIFT', 'RSHIFT', 'BOX' )



# Tokens

t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_LT      = r'<'
t_GT      = r'>'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_AND     = r'&'
t_OR	  = r'\|'
t_QUOT    = r'"'
t_TICK 	  = r'\''
t_HASH	  = r'\#'
t_LBRACKET= r'\['
t_RBRACKET= r'\]'
t_LBRACE  = r'\{'
t_RBRACE  = r'\}'
t_COMMA   = r','
t_PERIOD  = r'\.'
t_SEMIC   = r';'
t_COLON   = r':'
t_ARROW   = r'=>'
t_DDOT	  = r'\.\.'
t_DSTAR   = r'\*\*'
t_ASSIGN  = r':='
t_EQUALS  = r'='
t_NEQUALS = r'/='
t_GE      = r'>='
t_LE      = r'<='
t_LSHIFT  = r'<<'
t_RSHIFT  = r'>>'
t_BOX     = r'<>'

exponent  = r'([eE][-+]?[0-9]+)?'
p_exponent = r'(([eE][+]?)[0-9]+)?'
integer = r'[0-9](_?[0-9])*' + p_exponent
_float    = integer + '\.' + integer + exponent


def t_COMMENT(t):
	r'--.*'
	pass

def t_CHAR(t):
	r'\'[^\r\n]\''
	return t

def t_STRING(t):
	r'"[^"\\\r\n]*(\\.[^"\\\r\n]*)*"'
	return t

@TOKEN(_float)
def t_FLOAT(t):
	t.value = t.value.replace("_", "")
	return t

@TOKEN(integer)
def t_INTEGER(t):
	try:
		t.value = t.value.replace("_", "")
		t.value = int(float(t.value))
		t.value = str(t.value)
	except ValueError:
		print "Integer value too large %s" % t.value
		t.value = '0'
	return t
    
def t_IDENTIFIER(t):
	r'[a-zA-Z](_?[a-zA-Z0-9])*'
	t.type = rwords.get(t.value.lower(),'ID') 
	if t.type == 'ID':
		t.type = att.get(t.value.lower(),'ID') 
    
    #if t.type == 'ID' and self.type_lookup_func(t.value): #not a keyword and identifier is in the symbol table
	if t.type == 'ID':
		t.type = "TYPEID"
	return t

#def t_ATTRIBUTE(t):
#	r'\'[a-zA-Z]+'
#	t.type = att.get(t.value.lower(),'ID')
#	
#	if t.type == 'ID':
#		t.type = "TYPEID"
#	return t

t_ignore = " \t"

def t_newline(t):
	r'\n+'
	t.lexer.lineno += t.value.count("\n")
    
def t_error(t):
	print("Illegal character '%s'" % t.value[0])
	t.lexer.skip(1)

lexer = lex.lex()
f = open('test.adb', 'r')
for line in f:
	lexer.input(line)
	while True:
		tok = lexer.token()
		if not tok:
			break
		print tok

