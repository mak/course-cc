#include "tokenizer.h"
#include <cstdio>
#include <ctype>

reader::reader( )
{
   lookahead = getchar( );
      // From standard input.
   linenumber = 1;
}


bool reader::hasmore( ) const
{
   return lookahead != EOF;
}


void reader::moveforward( )
{
   // One should not read beyond end of file.

   if( hasmore( )) 
   {
      if( lookahead == '\n' )
         ++ linenumber;
      lookahead = getchar( );
   }
}


////////////////////////////////////////////////


// Such kind of code is very boring, and you will be tempted not to write
// it. But write it anyway.

std::ostream& operator << ( std::ostream& stream, const token& t )
{
   stream << "token( ";
   switch( t. t )
   {
   case tkn_double:
      stream << "double"; break;
   case tkn_int:
      stream << "int"; break;
   case tkn_char:
      stream << "char"; break;
   case tkn_string:
      stream << "string"; break;

   case tkn_plus:
      stream << "plus"; break;
   case tkn_minus:
      stream << "minus"; break;
   case tkn_times:
      stream << "times"; break;
   case tkn_div:
      stream << "div"; break;

   case tkn_lpar:
      stream << "lpar"; break;
   case tkn_rpar:
      stream << "rpar"; break;
   case tkn_laccolade:
      stream << "left accolade"; break;
   case tkn_raccolade:
      stream << "right accolade"; break;
   
   case tkn_error:
      stream << "error"; break;
   case tkn_eof:
      stream << "eof"; break;

   default:
      stream << "unknown type" << ( static_cast< unsigned int > (t.t) );
      stream << ", ";
         // Without this cast you would have an unbounded recursion here.
   }

   // Next we are going to print attributes.

   for( std::list< double > :: const_iterator
           p = t. doubleattr. begin( );
           p != t. doubleattr. end( );
           ++ p )
   {
      stream << ", " << *p;
   }

   for( std::list< unsigned int > :: const_iterator
           p = t. intattr. begin( );
           p != t. intattr. end( );
           ++ p )
   {
      stream << ", " << *p;
   }

   for( std::list< std::string > :: const_iterator
           p = t. stringattr. begin( );
           p != t. stringattr. end( );
           ++ p )
   {
      stream << ", " << *p;
   }

   stream << " )";
   return stream; 
}



// If you read end-of-file, you should return an eof token.
// If you cannot read a correct token, you should return an error token.
// In that case, you should always call moveforward( )
// because otherwise the program will hang.
// (It is quite easy to get this behaviour with >> in C++ )


token nexttoken( reader& r )
{
   if( r. lookahead == EOF )
      return token::token( tkn_eof );
         // No call to moveforward in this case.

   if( r. lookahead == '+' )
   {
      r. moveforward( );
      return token::token( tkn_plus );
   }

   if( r. lookahead == '-' )
   {
      r. moveforward( );
      return token::token( tkn_minus );
   }

   // Could not read a token.
   // We put the offending character in the string attribute.

   token t = tkn_error;

   t. stringattr. push_back( std::string( ));
   t. stringattr. back( ) += r. lookahead; 
   
   r. moveforward( );
   return t;
}

inline bool isHex (int c)
{
   isxdigit(c) ? true : false;
}

inline bool isNum (int c)
{
   isdigit(c) ? true : false;
}

token hex (reader &r)
{
   std::list<int> l;
   int c,n=0;
   if(r.lookahead == '$')
   {
      r.moveforward();
      c= r.lookahead;
      for(;isHex(tmp);c=r.lookahead)
      {
	 if(isNum(c))
	    l.push_back(c - 0x30);
	 else
	    l.push_back(tolower(c) - 0x37);
      }
      c = 1;
      if(l.begin() != l.end())
      {
	for(std::list<int>::iterator it=l.begin(); it!=l.end();++it,c*=16)
	  n += c * (*it);
	token t = tkn_int;
	t.intattr.push_back(n);
	return t;
      }
   }   
   token t = tkn_error;
   r. moveforward( );
   return t;
}

// tokenize comments, strip it 
token comment (reader &r)
{
   bool star_flag = false;
   //std::list<token> t_list;
   if (r.lookahead == '/')
   {
      r.moveforward();
      if(r.lookahead == '*')
      {
//	 t_list.push_back( token :: token(tkn_lcomm));
	 r.moveforward();
	 while( star_flag && r.lookahead == '/')
	 {
	    if (r.lookahead == '*')
	       star_flag = true;
	    r.moveforward();
	 }
	 return token::token(tkn_comm);
      }
   }
   token t = tkn_error;
   r. moveforward( );
   return t;
} 

int main( int argc, char* argv [ ] )
{

#if 0 
   // Simple test for the reader.

   reader r;
   while( r. hasmore( ))
   {
      std::cout << r. lookahead;
      r. moveforward( );
   }

#endif 

#if 0
   // We make a few tokens and see if they are printed well.

   token t = tkn_double;
   t. doubleattr. push_back( 3.1415 );
   t. stringattr. push_back( "hello-world" );
   std::cout << t << "\n";

#endif

#if 1
   // We read tokens until end of file is reached.

   reader r;
   token t = nexttoken(r);
   while( t. t != tkn_eof )
   {
      std::cout << t << "\n";
      t = nexttoken(r);
   }
   std::cout << t << "\n"; 
#endif 

   return 0;
}

