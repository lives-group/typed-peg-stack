\documentclass[sigplan,screen]{acmart}
\let\Bbbk\undefined
%include polycode.fmt

\usepackage{booktabs} % For formal tables
\usepackage[utf8x]{inputenc}
\usepackage{ucs}
\usepackage{graphicx}
\usepackage{amsmath,amsthm,amsfonts}
\usepackage{amssymb}
\usepackage{url}
\usepackage{stmaryrd}
\usepackage{ifpdf}
\ifpdf
  \usepackage{hyperref}
\fi
\usepackage{float}
\usepackage{proof}
%if False
\begin{code}
module paper where
\end{code}
%endif
%\renewcommand{\shortauthors}{Daher et al.}
% Copyright
%\setcopyright{none}
%\setcopyright{acmcopyright}
%\setcopyright{acmlicensed}
%\setcopyright{rightsretained}
%\setcopyright{usgov}
%\setcopyright{usgovmixed}
%\setcopyright{cagov}
%\setcopyright{cagovmixed}

\renewcommand\abstractname{ABSTRACT}
\renewcommand\keywordsname{KEYWORDS}
\renewcommand\refname{REFERENCES}
\renewcommand\footnotetextcopyrightpermission[1]{} % removes footnote with conference information in first column
%Conference

\copyrightyear{2025}
\acmYear{2025}
\acmConference[SBLP'25]{XXIX Brazilian Symposium on Programming Languages}{September 22–26, 2025}{Recife, PE}

%include lhs2TeX.fmt
%include lhs2TeX.sty

\DeclareMathAlphabet{\mathkw}{OT1}{cmss}{bx}{n}
%subst keyword a = "\mathkw{" a "}"
%subst conid a = "\V{" a "}"
%subst varid a = "\V{" a "}"
%subst numeral a = "\Con{" a "}"

\newtheorem{Lemma}{Lemma}
\newtheorem{Theorem}{Theorem}
\theoremstyle{definition}
\newtheorem{Example}{Example}

\usepackage{color}
\newcommand{\redFG}[1]{\textcolor[rgb]{0.6,0,0}{#1}}
\newcommand{\greenFG}[1]{\textcolor[rgb]{0,0.4,0}{#1}}
\newcommand{\blueFG}[1]{\textcolor[rgb]{0,0,0.8}{#1}}
\newcommand{\orangeFG}[1]{\textcolor[rgb]{0.8,0.4,0}{#1}}
\newcommand{\purpleFG}[1]{\textcolor[rgb]{0.4,0,0.4}{#1}}
\newcommand{\yellowFG}[1]{\textcolor{yellow}{#1}}
\newcommand{\brownFG}[1]{\textcolor[rgb]{0.5,0.2,0.2}{#1}}
\newcommand{\blackFG}[1]{\textcolor[rgb]{0,0,0}{#1}}
\newcommand{\whiteFG}[1]{\textcolor[rgb]{1,1,1}{#1}}
\newcommand{\yellowBG}[1]{\colorbox[rgb]{1,1,0.2}{#1}}
\newcommand{\brownBG}[1]{\colorbox[rgb]{1.0,0.7,0.4}{#1}}

\newcommand{\ColourStuff}{
  \newcommand{\red}{\redFG}
  \newcommand{\green}{\greenFG}
  \newcommand{\blue}{\blueFG}
  \newcommand{\orange}{\orangeFG}
  \newcommand{\purple}{\purpleFG}
  \newcommand{\yellow}{\yellowFG}
  \newcommand{\brown}{\brownFG}
  \newcommand{\black}{\blackFG}
  \newcommand{\white}{\whiteFG}
}

\newcommand{\MonochromeStuff}{
  \newcommand{\red}{\blackFG}
  \newcommand{\green}{\blackFG}
  \newcommand{\blue}{\blackFG}
  \newcommand{\orange}{\blackFG}
  \newcommand{\purple}{\blackFG}
  \newcommand{\yellow}{\blackFG}
  \newcommand{\brown}{\blackFG}
  \newcommand{\black}{\blackFG}
  \newcommand{\white}{\blackFG}
}

\ColourStuff

\newcommand{\D}[1]{\blue{\mathsf{#1}}}
\newcommand{\Con}[1]{\red{\mathsf{#1}}}
\newcommand{\F}[1]{\green{\mathsf{#1}}}
\newcommand{\V}[1]{\purple{\mathit{#1}}}

\setcopyright{none}
\settopmatter{printacmref=false} % Removes citation information below abstract

%subst comment a = "\orange{\texttt{--" a "}}"

\begin{document}

\title{Pest control: A formal model of the Pest parser generator}

\begin{abstract}
Parsing expressions grammars (PEGs) are a recognition-based formalism for parsers,
which has been gaining popularity because they avoid some problems present in context-free grammar
based specifications. In the context of the Rust programming language, the most used
PEG-based parser generator is Pest. An interesting feature of Pest grammars is that they
support new operators for repetition and handling an implicit stack during parsing.
In this work, we propose a formalization of these new constructs and extend a
type inference algorithm which guarantee that all well-typed Pest-style grammars
do not loop on inputs.
\end{abstract}

\author{Guilherme Daher}
\email{guilherme.daher@@aluno.ufop.edu.br}
%\orcid{0000-0003-4174-8616}
\affiliation{%
  \department{Prog. de P\'os-Gradua\c{c}\~ao em Ci\^encia da Computa\c{c}\~ao}
  \institution{Universidade Federal de Ouro Preto}
  \city{Ouro Preto}
  \state{Minas Gerais}
  \country{Brazil}
}

\author{Elton Cardoso}
\email{eltonmc@@ufop.edu.br}
%\orcid{0000-0003-4174-8616}
\affiliation{%
  \department{Prog. de P\'os-Gradua\c{c}\~ao em Ci\^encia da Computa\c{c}\~ao}
  \institution{Universidade Federal de Ouro Preto}
  \city{Ouro Preto}
  \state{Minas Gerais}
  \country{Brazil}
}

\author{Leonardo Reis}
\email{lvsreis@@ice.ufjf.br}
%\orcid{0000-0002-1167-5332}
\affiliation{%
  \department{Departamento de Ci\^encia da Computa\c c\~ao}
  \institution{Universidade Federal de Juiz de Fora}
  \city{Juiz de Fora}
  \state{Minas Gerais}
  \country{Brazil}
}


\author{Rodrigo Ribeiro}
\email{rodrigo.ribeiro@@ufop.edu.br}
%\orcid{0000-0003-0131-5154}
\affiliation{%
  \department{Prog. de P\'os-Gradua\c{c}\~ao em Ci\^encia da Computa\c{c}\~ao}
  \institution{Universidade Federal de Ouro Preto}
  \city{Ouro Preto}
  \state{Minas Gerais}
  \country{Brazil}
}



\maketitle

\section*{KEYWORDS}

Parsing, Parsing Expression Grammars, Type systems


%format . = "."
%format Set = "\D{Set}"
%format Set0 = Set "_{\D{0}}"
%format Set1 = Set "_{\D{1}}"
%format List = "\D{List}"
%format [] = "\Con{\lbrack\:\rbrack}"
%format , = "\red{,}\,"
%format Nat = "\D{\mathbb{N}}"
%format zero = "\Con{zero}"
%format succ = "\Con{suc}"
%format id = "\F{id}"
%format o = "\F{\circ}"
%format Vec = "\D{Vec}"
%format :: = "\Con{::}"
%format _::_ = "\Con{\_::\_}"

\section{Introduction}\label{sec:intro}

Parsing is a core problem in computer science. In simple terms, a parsing
algorithm, also named a parser, checks if a string of symbols conforms to some
set of rules specified by formalisms like context-free grammars (CFGs) or
regular expressions (REs). Usually, parsers construct data structures showing
what rules were used to conclude that the input string was obtainable from the
specification or an error message indicating that such string is unobtainable
from the specification. Due to its importance, parsing is an extensively studied
subject, as evidenced by Grune and Jacob's book "Parsing Techniques: A Practical
Guide", which has more than 600 pages and covers only classic algorithms \cite{Grune90}.
However, parsing is far from being a solved problem: in recent years,
we have seen a growing interest in the development of new algorithms and
formalisms~\cite{Ford2004, Trevor10,Asperti10, Lucks17, Zhang23} or the verification
of well-known parsing techniques ~\cite{FirsovU14, FirsovU13,LopesRC16,Bernardy16}.

In the early 2000's, Brian Ford proposed \emph{Parsing Expression Grammars}
(PEGs),  a recognition-based formalism to
define languages~\cite{Ford2004}. The novelty of PEGs is that they
specify how to check whether a string is in
the language being defined and not how it is generated by its specification.
Formally a PEG is a 4-tuple $(V_N, V_T, R, e_S)$ where $V_N$ and $V_T$ are
as in CFGs, $R$ is a function which maps a non terminal
from $V_N$ to a {\it parsing expression} and $e_S$ is the initial parsing
expression. A parsing expression is very
similar to the right-hand side of a CFG in the extended Backus-Naur
form with the addition of new operators such as the not-predicate (!) and
the prioritized choice (/). A fundamental difference between PEG
and CFGs is the choice operator, which impose priority between alternatives.
It means that $\beta$ in a prioritized choice expression $\alpha / \beta$ is tested only
in case of failure on $\alpha$. The not-predicate operator checks
if the string matches some syntax without consuming the input.

In the context of the Rust programming language~\cite{RustBook}, the
Pest library~\cite{PEST} is the most popular PEG-based parser
generator for Rust, having more than 114.000 downloads registered at
\href{http://crates.io}{crates.io} package registry. Besides supporting
all traditional PEG operators, Pest support operators to manipulate an
implicit stack during parser execution. The inclusion of a stack allows
Pest parsers to elegantly express identation sensitive languages by
controlling identation levels using the stack. While such new operators
eases the task of writing grammars for complex formats, additional care
must be taken in order to ensure PEG \emph{well-formedness}. We
say that a PEG is well-formed if it does not loop on any input
strings~\cite{Ford2004}. Pest users have reported that the tool
fails to detect grammars that loop on some inputs~\cite{PESTBUG}.
In this work, we propose a formalization of a generalized version
of these Pest operators using operational semantics and
extend a type system for PEGs~\cite{Ribeiro19} and its
inference algorithm~\cite{Cardoso23} to deal with these operators.

Specifically, we make the following contributions:

\begin{itemize}
  \item We formalize both stack and the new repetitions Pest operators using
    operational semantics and extend a type system for PEGs~\cite{Ribeiro19}
    to deal with these new operations. The proposed semantics is realized by a
    intrisincally-typed definitional interpreter~\cite{Poulsen17} using the
    Agda programming language~\cite{Norell08}. While the semantics is formalized
    in Agda, we present and describe it using~\LaTeX~notation. We choose this
    presentation style for brevity and to not obsfucate the semantics core ideas
    with syntax details of the Agda language. The interested reader can check
    the formalization source code on-line~\cite{pegrepository}.
  \item We include operations to query the top-most element of the stack
    which can be used to handle context-sensitive formats, like length
    indexed fields, common in binary data formats;
  \item We show how to extend a type inference algorithm~\cite{Cardoso23} for PEGs
    to deal with Pest new operators. A prototype implementation of the inference
    algorithm was developed using the Racket programming language~\cite{Culpepper19}
    and used to check some example grammars.
\end{itemize}

The rest of this paper is organized as follows.
Section~\ref{sec:pest-operators} presents an informal description of Pest operators.
Section~\ref{sec:background}
reviews the syntax, semantics and a type system for
PEGs. In Section~\ref{sec:pest-formalization} defines the syntax, semantics, the type
system and its inference algorithm for the new PEG operators. Section~\ref{sec:examples}
presents some use examples of grammars accepted by our tool.
Related work is discussed on Section~\ref{sec:related} and
Section~\ref{sec:conclusion} concludes.


\section{An overview of Pest new operators}~\label{sec:pest-operators}

Before starting the description of each new operator, we describe
\emph{repetition indexes}, which are values ranged over natural numbers and a
special value, \textbf{infty}, which denotes an upper-bound over
indexes\footnote{For any index $i \neq \mathbf{infty}$, we have
that $i < \mathbf{infty}$ and $i \circ \mathbf{infty} = \infty$}.
Notation $\circ$ denotes either addition
or multiplication operations over natural numbers. The constants
\textbf{top.tonat} and \textbf{top.length} refer to the conversion
of the stack top element to a numeric value and its size, respectively.
More details on how these operations work will be described
on the semantics.

The first operator we consider is the iterated repetition, denoted $e^i$ in
the abstract syntax and \verb|e^i| in the concrete syntax. It describes that
expression $e$ should be repeated $i$ times over the input, failing if any of these iterations
results in a parser failure. As an example of the usefulness of this operator, consider
the task of representing a PEG for a format formed by a two bytes values followed by
a single bit terminator. We could express this simple format as the following
grammar using the iterated repetition:
\begin{verbatim}
bit <-- '0' / '1'
byte <-- bit^8
start: byte^2 ~ bit
\end{verbatim}
Notation \verb|'0'| denotes an expression for a string,
operator \verb|~| denotes concatenation (following Pest concrete syntax),
\verb|A <-- e| denotes a grammar rule and \verb|start:| specifies
the starting expression for this grammar. The usage of the iterated
repetition operator allows for concise description of pattern ``2 bytes''
as simply \verb|byte^2|, which indicates that non terminal \verb|byte|
should be repeated two times over the input.

The Pest tool also introduces an interval repetition operator, $e[i,j]$,
which executes the expression $e$ a number of times between the indexes $i$
and $j$, inclusive. As an example of this operator usage, consider the
task of parsing either 2,3 or 4 bits sequences from input. This could be
expressed elegantly by the following expression using the interval repetition:
\verb|('0' / '1')[2,4]|.

However, not all combinations of indexes are allowed in repetition operators.
An example of an invalid repetition operator: \verb|e[infty, 2]|, since a
bounded repetition, \verb|e[i,j]|, is valid if $i < j$ and $i$ is not the constant
\verb|infty|. Regarding the exact repetition operator, $e^i$, we do not allow to use
the index \textbf{infty}, since its semantics is to parse the input by repeating
an expression $e$ \textbf{exactly} $i$ times, which makes the expression
$e^{\mathbf{infty}}$ to be repeat indefinitely over the input.

Now, we turn our attention to the stack manipulation operators. Expression
\verb|push(e)| parses the input using the expression \texttt{e} and pushes the
consumed input on the parser stack. Operator \verb|pop| removes the current
stack top-most element and tries to match it on the current input,
\verb|peek| also matches the input without removing the element from the
stack and \verb|drop| removes the first element of current stack, without
matching it. The operator \verb|peekall| tries to match the input using the
complete stack content, without empty the stack. Finally, \verb|dropall|
empties the stack.

One interesting example use of stack operators is to parse
length-indexed fields present in several data formats.
The next grammar shows how to express netstrings, which is a format method
for strings which contains a field to denote the size of the represented data.
\begin{verbatim}
number <-- '0' / '1' / ... / '9'
letter <-- 'a' / ... / 'z'
char <-- number / letter

start: push(number+) ~ ':' ~ char^top.tonat ~ ','
\end{verbatim}
The starting expression begins by pushing the input size value on the parser stack,
and then access this size information using \verb|top.tonat| to parse the exact
quantity of input based on the size information that starts the netstring.
Operators \verb|top.tonat| and \verb|top.length| are not present in the current
definition of Pest parser generator. We include both since they have a direct
formalization and increases the expressivity for Pest grammars which can be used
to represent length-indexed fields, which are notoriously difficult to parse
correctly~\cite{Zhang23, Lucks17}.


\section{Background}\label{sec:background}

\paragraph{An Overview of PEGs}

Intuitively, PEGs are a formalism for describing top-down parsers.
Formally, a PEG is a 4-tuple $(V,\Sigma,R,e_S)$, where $V$ is a
finite set of variables, $\Sigma$ is the alphabet, $R$ is the finite
set of rules, and $e_s$ is the start expression. Each rule $r \in R$ is a
pair $(A,e)$, usually written $A \leftarrow e$, where $A \in V$ and $e$
is a parsing expression. We let the meta-variable $a$ denote an
arbitrary alphabet symbol, $A$ a variable and $e$ a parsing expression.
Following common practice, all meta-variables can appear primed or
sub-scripted. The following context-free grammar defines
the syntax of a parsing expression:
\[
\begin{array}{lcl}
e & \to  & \epsilon \,\mid\, a \,\mid\, A\,\mid\,e_1\:e_2\,
\mid\,e_1\,/\,e_2\,\mid\,e^\star\,\mid\,!\,e\\
\end{array}
\]
The execution of parsing expressions is defined by an inductively defined
judgment that relates pairs formed by a parsing expression and an input string
to pairs formed by the consumed prefix and the remaining string.
Notation $(e,s) \Rightarrow_G (s_p,s_r)$ denote that parsing expression $e$
consumes the prefix $s_p$ from the input string $s$ leaving the suffix $s_r$.
The notation $(e,s) \Rightarrow_G \bot$ denote the fact that $s$ cannot be parsed by
$e$. We let meta-variable $r$ denote an arbitrary parsing result, i.e.,
either $r$ is a pair $(s_p,s_r)$ or $\bot$. We say that an expression $e$
fails if its execution over an input produces $\bot$; otherwise, it succeeds.
Figure~\ref{fig:pegsemantics} defines the PEG semantics.
\begin{figure*}[h!]
   \[
      \begin{array}{cccc}
         \infer[_{\{Eps\}}]{(\epsilon,s)\Rightarrow_G (\epsilon,s)}{} &
         \infer[_{\{ChrS\}}]{(a,as_r) \Rightarrow_G (a,s_r)}{}  &
         \infer[_{\{ChrF\}}]{(a,bs_r) \Rightarrow_G \bot}{a \neq b} &
         \infer[_{\{Var\}}]{(A,s) \Rightarrow_G r}
                           {A \leftarrow e \in R & (e,s) \Rightarrow_G r} \\ \\
         \multicolumn{2}{c}{
            \infer[_{\{Cat_{S1}\}}]{(e_1\,e_2,s_{p_1}s_{p_2}s_r) \Rightarrow_G (s_{p_1}s_{p_2}, s_r)}
                                 {(e_1,s_{p_1}s_{p_2}s_r) \Rightarrow_G (s_{p_1},s_{p_2}s_r) &
                                 (e_2,s_{p_2}s_r)\Rightarrow_G (s_{p_2},s_r)}
         } &
         \multicolumn{2}{c}{
            \infer[_{\{Cat_{F2}\}}]{(e_1\,e_2,s_ps_r) \Rightarrow_G \bot}
                                 { (e_1,s_ps_r) \Rightarrow_G (s_p,s_r) &
                                    (e_2,s_r) \Rightarrow_G \bot}} \\ \\
         \infer[_{\{Cat_{F1}\}}]{(e_1\,e_2,s)\Rightarrow_G \bot}{(e_1,s) \Rightarrow_G \bot} &
         \infer[_{\{Alt_{S1}\}}]{(e_1\,/\,e_2,s_p\,s_r) \Rightarrow_G (s_p,s_r)}
                                {(e_1,s_p\,s_r)\Rightarrow_G (s_p,s_r)} &
         \multicolumn{2}{c}{
            \infer[_{\{Alt_{S2}\}}]{(e_1\,/\,e_2,s_p\,s_r) \Rightarrow_G r}
                                  {(e_1,s_p\,s_r)\Rightarrow_G \bot &
                                   (e_2,s_p\,s_r)\Rightarrow_G r}
         } \\ \\
         \multicolumn{2}{c}{
            \infer[_{\{Star_{rec}\}}]{(e^\star,s_{p_1}s_{p_2}s_r) \Rightarrow_G (s_{p_1}s_{p_2},s_r)}
                                 {(e,s_{p_1}s_{p_2}s_r) \Rightarrow_G (s_{p_1},s_{p_2}s_r) &
                                  (e^\star, s_{p_2}s_r) \Rightarrow_G (s_{p_2},s_r)}
         } &
         \multicolumn{2}{c}{
            \infer[_{\{Star_{end}\}}]{(e^\star,s) \Rightarrow_G (\epsilon,s)}
                                    {(e,s) \Rightarrow_G \bot}} \\ \\
         \multicolumn{2}{c}{
            \infer[_{\{Not_F\}}]{(!\,e,s_p\,s_r) \Rightarrow_G \bot}
                               {(e,s_p\,s_r) \Rightarrow_G (s_p,s_r)}
         } &
         \infer[_{\{Not_S\}}]{(!\,e,s) \Rightarrow_G (\epsilon,s)}
         {(e,s) \Rightarrow_G \bot}
           &
         \infer[_{\{ChrNil\}}]{(a,\epsilon) \Rightarrow_G \bot}{}
      \end{array}
   \]
   \centering
   \caption{Parsing expressions operational semantics.}
   \label{fig:pegsemantics}
\end{figure*}
We comment on some rules of the semantics. Rule $_{Eps}$ specifies that
expression $\epsilon$ will not fail on any input $s$ by leaving it unchanged.
Rule $_{ChrS}$ specifies that an expression $a$ consumes the first character when
the input string starts with an `a' and rule $_{ChrF}$ shows that it fails when
the input starts with a different symbol. Rule $_{Var}$ parses the input using
the expression associated with the variable in the grammar $G$. When parsing a
sequence expression, $e_1\:e_2$, the result is formed by $e_1$ and $e_2$ parsed
prefixes and the remaining input is given by the result $e_2$. Rules $_{Cat_{F1}}$ and
$_{Cat_{F2}}$ say that if $e_1$ or $e_2$ fail, then the whole expression fails.
The rules for choice impose that we only try
expression $e_2$ in $e_1 / e_2$ when $e_1$ fails. Parsing a star
expression $e^\star$ consists in repeatedly execute $e$ on the input string.
When $e$ fails, $e^\star$ succeeds without consuming any symbol of the input
string. Finally, the rules for the not-predicate expression, $!\,e$, specify
that whenever the expression $e$ succeeds on input $s$, $!\,e$ fails; and when $e$
fails on $s$ we have that $!\,e$ succeeds without consuming any input.

\paragraph{A type system for PEGs.}

A key issue in PEG formalism is to ensure the termination of the parsing algorithm
for arbitrary inputs. Ford~\cite{Ford2004} proposed a well-formedness predicate
which is computed as fixed-point over the set of grammar's sub-expressions.
In simple terms, a PEG can loop indefinitely over an input string if: 1) it has
direct or indirect left-recursive rules ; 2) expressions that succeed without
consuming any symbol under the Kleene star operator.
As pointed by~\cite{Ribeiro19}, one inconvenience of Ford's well-formedness predicate
is that it is not compositional.
Based on this observation,~\citet{Ribeiro19} propose a type
system that guarantees termination by restricting valid expressions to those
that do not obey conditions 1 and 2. In order to ensure these restrictions,
types are interpreted as records containing two fields:
a boolean value indicating that the current expression may succeed without
consuming anything from the input string and a set of variables that can
appear as the first symbol on the recognition branch from the current parsing
expression. This set (named head-set) is defined by recursion on the parsing
expression syntax as follows:
\[
  \begin{array}{lcl}
    head (\epsilon) & = & \emptyset \\
    head (a) & = & \emptyset \\
    head (A) & = & \{A\} \cup head(R(A)) \\
    head (e_1\,e_2) & = & \left\{
                          \begin{array}{ll}
                            head(e_1) \cup head(e_2) & \text{if }e_1 \rightharpoonup 0\\
                            head(e_1) & \text{otherwise}
                          \end{array}
                                        \right. \\
    head(e_1\,/\,e_2) & = & head(e_1) \cup head(e_2) \\
    head(e^\star) & = & head(e) \\
    head(!\,e)    & = & head(e)
  \end{array}
\]
Basically, the head-set of $\epsilon$ and $a \in \Sigma$ is the empty set.
For a non terminal, the head-set is formed by the non terminal itself plus
the head-set of its associated parsing expression. The head-set of a sequence
expression $e_1\:e_2$ need to include the head-set of $e_2$, whenever $e_1$
succeeds without consuming anything ($e_1 \rightharpoonup 0$). Otherwise,
$head(e_1\:e_2)$ is just $head(e_1)$. The set of non terminals for the choice
operator is the union of the head-set of its operands. Finally, the head-set
for the Kleene star and not operator are just the set for its underlying expressions.
Following Ribeiro et. al., $\tau$ denotes an arbitrary type, $\tau.null$
denotes the boolean field of $\tau$, and $\tau.head$ the set of
variables that can appear as the first symbol of $\tau$'s parsing expression.
We let notation $\langle b , S \rangle$ denote a type $\tau$ formed by a
boolean $b$ and a set $S$. In the type system, Ribeiro et. al use the following operations:
\[
  \begin{array}{l}
    b \Rightarrow S = \text{if } b \text{ then } S \text{ else }\emptyset\\
    \tau_1 \otimes \tau_2 = \langle \tau_1.null \land \tau_2.null,
                                \tau_1.head \cup (\tau_1.null \Rightarrow \tau_2.head) \rangle \\
    \tau_1 \oplus \tau_2 = \langle \tau_1.null \lor \tau_2.null,
                                \tau_1.head \cup \tau_2.head\rangle\\
    !\,\tau = \langle true , \tau.head \rangle\\
    \langle false, S \rangle^\star = \langle true, S \rangle
  \end{array}
\]
The first operation is a boolean test that returns the set $S$
whenever the condition is true. The product operation ($\tau_1 \otimes \tau_2$) combines two
types by taking the conjunction of their boolean flags and the union of their respective
head-sets in case $\tau_1.null$ is true. The co-product operation ($\tau_1 \oplus \tau_2$) on types is similar to the
product, but instead of using conjunction it takes the disjunction of their boolean fields.
The not operation on types just change its boolean field to true and keep its head-set.
The last operation is the Kleene star on types, which also sets its boolean field to true.
However, the Kleene is only defined for types $\tau$ such that $\tau.null = false$, being
undefined when $\tau.null = true$. Finally, following the common practice, the meta-variable
$\Gamma$ denotes a typing context and notation $\Gamma(A) = \tau$ holds whenever
$(A,\tau) \in \Gamma$.
The type system is defined as an inductive judgment
$\Gamma \vdash e : \tau$, where $\Gamma$ is a typing context, $e$ is a parsing expression
and $\tau$ is a type (Figure~\ref{fig:typesystem}). A
PEG $G = (V,\Sigma,R,e_S)$ is considered well-typed, written $\Gamma \vdash G$,
if $\forall A \in V . R(A) : \Gamma (A)$.

\begin{figure}[h!]
\[
  \begin{array}{cc}
    \infer{\Gamma \vdash \epsilon : \langle true , \emptyset \rangle}{} &
    \infer{\Gamma \vdash a : \langle false , \emptyset\rangle}{} \\ \\
    \multicolumn{2}{c}{
      \infer{\Gamma \vdash A : \langle \tau.null, \tau.head \cup {A}\rangle}
            {\Gamma(A) = \tau & A \not\in \tau.head}
    }\\ \\
    \infer{\Gamma\vdash e_1\,/\,e_2 : \tau_1 \oplus \tau_2}
          {\Gamma \vdash e_1 : \tau_1 & \Gamma \vdash e_2 : \tau_2}
    &
    \infer{\Gamma\vdash !\,e :\, !\,\tau}
          {\Gamma \vdash e : \tau}\\ \\
    \infer{\Gamma\vdash e_1\,e_2 : \tau_1 \otimes \tau_2}
          {\Gamma \vdash e_1 : \tau_1 & \Gamma \vdash e_2 : \tau_2} &
    \infer{\Gamma\vdash e^\star : \tau^\star}
          {\begin{array}{c}
             \Gamma \vdash e : \tau\\
             \tau.null = false\\
           \end{array}}
  \end{array}
\]
\centering
\caption{Type system definition.}
\label{fig:typesystem}
\end{figure}

The first rule of the type system specifies that a parsing
expression $\epsilon$ has type $\langle true, \emptyset \rangle$.
The rule of a single character expression says that its type is
$\langle false, \emptyset\rangle$, since it does not succeed without
consuming a symbol and no variable can appear as the head of its
recognition branch, i.e., its head-set is $\emptyset$. The type of a prioritized
choice expression ($e_1/e_2$) has type $\tau_1 \oplus \tau_2$,
where $\Gamma \vdash e_1 : \tau_1$ and $\Gamma \vdash e_2 : \tau_2$.
The type of a sequence expression is given by the product operation
on their sub-expressions types, and the star operation is only well-typed
if its underlying expression does not succeed without consuming
a symbol. A not-predicate parsing expression always has the type
$\langle true,\tau.head \rangle$ whenever its sub-expression is well-typed.
Finally, a variable is well-typed only if its type is in the context and
itself does not belong to its head-set.
\citet{Ribeiro19} type system readily rejects invalid expressions like
$a(!b / c)^\star$, since it only considers well-typed a Kleene parsing
expressions that its underlying expression does
not succeed without consuming the input. However, an inconvenience of it is
the need to require type annotations on all grammar's non terminals.
A later work~\cite{Cardoso23} proposes a type inference algorithm, which
guarantee termination of the parsing process.

\section{Formalizing Pest operators}~\label{sec:pest-formalization}

% We start by presenting the syntax of the new PEG operators, together with
% some examples. Next, we present the formal semantics of the new operators,
% giving details on repetition indexes are normalized. Finally, we describe
% our extension of Ribeiro et. al. type system~\cite{Ribeiro19} which
% guarantee termination of grammars using the new operators.

\paragraph{New operators syntax}
The Pest tool includes two types of operators: stack and repetitions operators.
The syntax of these operators are defined next:
\[
\begin{array}{lcl}
  e & \to  & ...\,\mid\,e^i\,\mid\,e[i,i]\,\mid\,\mathbf{push}\,\mid\,\mathbf{pop}\,\mid\,\mathbf{peek}
             \,\mid\,\mathbf{drop}\\
    & \mid & \mathbf{peekall}\,\mid\,\mathbf{dropall}\\
  i & \to  & \mathbf{top.tonat}\,\mid\,\mathbf{top.length}\,\mid\,i \circ i\,\mid\,v\\
  v & \to  & \mathbf{infty}\,\mid\,n,m\in\mathbb{N}\\
  \sigma & \to & s : \sigma\,\mid\,[] \\
  \circ & \to & + \,\mid\, \times
\end{array}
\]
Meta-variable $s$ denotes an arbitrary string using grammar's alphabet.
We let meta-variable $\sigma$ denote the parser stack and we represent it
using Haskell-style list notation.

\paragraph{Semantics of new operators}
Now that we have introduced the syntax and the informal semantics of the new Pest
operators, we turn our attention to its formal semantics. One important aspect
of Pest repetition operators is the usage of indexes. Before evaluating any
of these new operators we need to reduce indexes to a normal form.
We consider that an index is in normal form if it is a number or the constant
\textbf{infty}. We let meta-variable $v$ denote an arbitrary index normal form.
In simple terms, the index normalization algorithm performs the following steps:

\begin{enumerate}
  \item Evaluate $\mathbf{top.length}$ and $\mathbf{top.tonat}$, if
    they are present in the current index. The usage of these operators
    can result in a run-time error if the parser stack is empty or the
    stack first element can not be converted to a number (for $\mathbf{top.tonat}$).
  \item Reduce all arithmetic operations using their usual meaning and
    the following reduction rules
    when the constant \textbf{infty} is present:
\[
  \begin{array}{c}
    n \circ \mathbf{infty} \rightarrow \mathbf{infty}\\
    \mathbf{infty}\circ n \rightarrow \mathbf{infty}\\
    \mathbf{infty}\circ\mathbf{infty} \rightarrow \mathbf{infty}\\
  \end{array}
\]
where $\circ$ denotes either addition or multiplication and $n$ a
numeric value.
\end{enumerate}

The semantics is denoted by the inductive judgment
(Figures~\ref{fig:newsemantics},~\ref{fig:exactsemantics} and~\ref{fig:intervalsemantics})
$(e, w, \sigma) \Rightarrow_G r$,
where $e$ is the current expression being executed, $w$ is the current input string,
$\sigma$ is the current input parser stack, $G$ is the grammar and $r$ is the output
which can be: 1) success, denoted by triple $(w_1,w_2,\sigma')$ where $w_1$
is the parsed prefix, $w_2$ is the remaining input and $\sigma'$ is the resulting
stack from executing the expression $e$, or; 2) failure, denoted by $\bot$.
The inclusion a stack requires changing the semantics of all standard PEG
operators to thread the current stack state through parser execution.
We omit these modified rules, since they don't add any interesting insight.
\begin{figure*}[h!]
  \[
    \begin{array}{ccc}
      \infer[_{\{PUSH_S\}}]
             {(\mathbf{push}\:e,w,\sigma) \Rightarrow_G (w_1, w_2, (w_1:\sigma'))}
             {(e,w,\sigma)\Rightarrow_G (w_1,w_2,\sigma')} &
      \infer[_{\{POP_S\}}]
            {(\mathbf{pop},w,w_1\,:\,\sigma)\Rightarrow_G (w_1,w_2,\sigma)}
            {\exists w_2. w = w_1w_2} &
        \infer[_{\{DROP_F\}}]
              {(\mathbf{drop},w,[])\Rightarrow_G \bot}{}\\
       & & \\
      \infer[_{\{POP_F\}}]
            {(\mathbf{pop},w,\sigma)\Rightarrow_G \bot}
            {\neg \exists w_1\,w_2\, \sigma'. w = w_1w_2 \land \sigma = w_1\,:\,\sigma'}
      &
      \multicolumn{2}{c}{
        \infer[_{\{PEEK_F\}}]
              {(\mathbf{peek},w,\sigma)\Rightarrow_G \bot}
              {\neg \exists w_1\,w_2\, \sigma'. w = w_1w_2 \land \sigma = w_1\,:\,\sigma'}
      }\\\\
      \infer[_{\{PEEK_S\}}]
              {(\mathbf{peek},w,w_1\,:\,\sigma)\Rightarrow_G (w_1,w_2,w_1 \,:\,\sigma)}
              {\exists w_2. w = w_1w_2} &

      \multicolumn{2}{c}{
        \infer[_{\{PEEKALL_S\}}]
              {(\mathbf{peekall},w,\sigma)\Rightarrow_G (w', w'',\sigma)}
              {\sigma = [w_0, ..., w_n] & w' = w_0...w_n & \exists w'' . w = w'w''}
      }\\
      & & \\
      \infer[_{\{DROP_S\}}]
            {(\mathbf{drop},w,w_1\,:\,\sigma)\Rightarrow_G (\epsilon,w,\sigma)}
            {}
              &
      \multicolumn{2}{c}{
        \infer[_{\{PEEKALL_F\}}]
              {(\mathbf{peekall},w,\sigma)\Rightarrow_G \bot}
              {\sigma = [w_1, ..., w_n] & w' = w_1...w_n & \neg \exists w'' . w = w'w''}
            }
         \\ & & \\
         \infer[_{\{PUSH_F\}}]
              {(\mathbf{push}(e),w,\sigma)\Rightarrow_G \bot}
              {(e,w,\sigma)\Rightarrow_G \bot} &
         \multicolumn{2}{c}{
             \infer[_{\{DROPALL_F\}}]
                   {(\mathbf{dropall}, w, [])\Rightarrow_G \bot}
                   {}
          } \\ & & \\
          \multicolumn{3}{c}{
           \infer[_{\{DROPALL_S\}}]
                 {(\mathbf{dropall}, w, w_1 : \sigma)\Rightarrow_G (\epsilon, w, [])}
                 {}
          }
  \end{array}
  \]
  \caption{Semantics of the stack operators}
  \label{fig:newsemantics}
\end{figure*}

Figure~\ref{fig:newsemantics} presents the semantics for the stack manipulation operators.
Rules $PUSH_S$ and $PUSH_F$ formalize the behaviour of the push operator, which pushes the
prefix parsed by the push's expression argument into the parser stack or it fails when
the expression fails. The semantics of the \textbf{pop} operator is defined by rules
$POP_S$ and $POP_F$, which removes the topmost element of the stack and matching it
against the input string. When such a matching is not possible, we have a failure.
The only difference between the semantics for the \textbf{peek} operation and \textbf{pop}
is that the former keeps the top element in the result stack.
The \textbf{drop} operator removes the top element of the stack without matching it
against the current input or it fails when the stack is empty (check rules $DROP_S$ and
$DROP_F$). Rules $PEEKALL_S$ and $PEEKALL_F$ uses the whole stack content to match
the current input, failing if all the stack elements do not form a prefix of the current
input and $DROPALL_S$ simply empties the parser stack.

\begin{figure*}[h!]
  \[
    \begin{array}{cccc}
      \infer[_{\{EX_{S1}\}}]
            {(e^i, w, \sigma)\Rightarrow_G (\epsilon,w,\sigma)}
            {\mathbf{norm}(i,\sigma) = 0} &
      \multicolumn{3}{c}{
        \infer[_{\{EX_{S2}\}}]
              {(e^i,w,\sigma) \Rightarrow_G (w_1w'_1,w'_2,\sigma')}
              {\begin{array}{c}
                \mathbf{norm}(i,\sigma) = n \\
                (e,w,\sigma)\Rightarrow_G(w_1,w_2,\sigma_1)
               \end{array} &
               \begin{array}{c}
                 0 < n \\
                 (e^{n - 1},w_2,\sigma_1)\Rightarrow_G (w'_1,w'_2,\sigma')
               \end{array}}
      } \\
      & & \\
          \infer[_{\{EX_{F1}\}}]
                {(e^i,w,\sigma)\Rightarrow_G \bot}
                {\begin{array}{c}
                  \mathbf{norm}(i,\sigma) = n \\
                  0 < n \quad (e,w,\sigma)\Rightarrow_G \bot
                \end{array}} &
          \multicolumn{3}{c}{
            \infer[_{\{EX_{F2}\}}]
                  {(e^i,w,\sigma) \Rightarrow_G \bot}
                  {\begin{array}{c}
                    \mathbf{norm}(i,\sigma) = n \\
                    (e,w,\sigma)\Rightarrow_G(w_1,w_2,\sigma_1)
                  \end{array} &
                  \begin{array}{c}
                    0 < n \\
                    (e^{n - 1},w_2,\sigma_1)\Rightarrow_G \bot
                  \end{array}}
          }
    \end{array}
  \]
  \caption{Semantics for the exact repetition operator}
  \label{fig:exactsemantics}
\end{figure*}

The semantics for the exact repetition operator is presented in
Figure~\ref{fig:exactsemantics}.
The first rule, $EX_{S1}$, shows that for any expression $e$, $e^0$ consumes the
empty string from the input and does not change the parser stack. The second rule,
$EX_{S2}$, shows that to evaluate $e^i$, we first normalize the index $i$ to obtain
a numeric value, $n$. Next, we check if $0 < n$ and parse the input using $e$ producing
a remaining suffix $w_2$, which is used as input for running the expression $e^{n - 1}$.
We say that the repetition operator $e^i$ fails if any iteration of $e$ results in a failure,
as formalized by rules $EX_{F1}$ and $EX_{F2}$.


\begin{figure*}[h!]
  \[
    \begin{array}{ccc}
      \infer[_{\{Inter_{1}\}}]
            {(e[i,j],w,\sigma) \Rightarrow_G r}
            {\begin{array}{c}
               \mathbf{norm}(i,\sigma) = n \\
               \mathbf{norm}(j,\sigma) = n \\
               (e^n,w, \sigma)\Rightarrow r
            \end{array}} &
      \infer[_{\{Inter_2\}}]
            {(e[i,j],w,\sigma)\Rightarrow_G (w_1,w_2,\sigma')}
            {\begin{array}{c}
              \mathbf{norm}(i,\sigma) = n \quad
              \mathbf{norm}(j,\sigma) = m \quad
              n < m \\
              (e^{m}, w, \sigma)\Rightarrow_G (w_1,w_2,\sigma')
          \end{array}} &
      \infer[_{\{Inter_{F1}\}}]
            {(e[i,j], w, \sigma) \Rightarrow_G \bot}
            {\begin{array}{c}
                \mathbf{norm}(i,\sigma) = n\\
                \mathbf{norm}(j,\sigma) = m\\
                n > m\\
            \end{array}} \\ & \\
       \multicolumn{3}{c}{
        \infer[_{\{Inter_3\}}]
              {(e[i,j], w, \sigma) \Rightarrow_G r}
              {\mathbf{norm}(i,\sigma) = n &
               \mathbf{norm}(j,\sigma) = m &
               n < m &
               (e^{m},w,\sigma)\Rightarrow_G \bot  &
               (e[n,m - 1],w, \sigma)\Rightarrow_G r}
        } \\ & \\
       \multicolumn{3}{c}{
          \infer[_{\{Inter_{I}\}}]
                {(e[i,j], w, \sigma) \Rightarrow_G r}
                {
                    \mathbf{norm}(i,\sigma) = n &
                    \mathbf{norm}(j,\sigma) = \mathbf{infty} &
                    (e^n\,e^{\star},w,\sigma)\Rightarrow_G r
                }
        }
    \end{array}
  \]
  \centering
  \caption{Semantics for the interval repetition operator}
  \label{fig:intervalsemantics}
\end{figure*}

The last set of semantics rules deals with interval repetition operator,
$e[i,j]$. Rule $Inter_{1}$ parses the input using $e^n$, when both of indexes
normalizes to the same numeric value $n$. The rule $Inter_2$ shows how to run
an interval repetition when the normalized indexes are such that $n < m$,
where $\mathbf{norm}(i,\sigma) = n$ and $\mathbf{norm}(j,\sigma) = m$:
it tries to parse the input using $e^m$, if it succeeds then result is
returned. In case of failure of $e^m$, rule $Inter_3$ applies  we
try to parse the input using the expression $e[n, m -1]$.
Rule $Inter_{F1}$ specifies that an interval repetition fails when the formalized value of $i$
is greater than normalized value of $j$.
The rule deal with the situation of the upper limit bound normalizes to \textbf{infty}.
In this situation, expression $e[i,j]$ is interpreted as $e^n e^*$, where $n$ is the
result of normalizing the index $i$.

\subsection{Type system and its inference algorithm}~\label{sec:typesystem}

Now, we turn our attention to extending a type system to avoid
non termination problems during a grammar execution. We follow
the approach of defining typing rules and its corresponding
type inference using the strategy of constraint generation
followed by a solving step~\cite{Ribeiro19,Cardoso23, Pottier05}.

Before starting discussing the typing rules for the new operators,
we need to extend the $head$ function. In Figure~\ref{fig:new-head-set},
we present the new equations which handle the both repetions and stack
PEG construction.

\begin{figure}[H]
  \[
    \begin{array}{lcl}
      head(e^i) & = & head(e)\\
      head(e[i,j]) & = & head(e)\\
      head(\mathbf{push}(e)) & = & head(e)\\
      head(\mathbf{pop}) & = & \emptyset\\
      head(\mathbf{peek}) & = & \emptyset\\
      head(\mathbf{drop}) & = & \emptyset\\
      head(\mathbf{dropall}) & = & \emptyset\\
      head(\mathbf{peekall}) & = & \emptyset\\
    \end{array}
  \]
  \caption{Head-set equations for the new operators}
  \label{fig:new-head-set}
\end{figure}
The first three equations shows that the head-set of
repetitions and the push operator are equal to their
parameter expression set. All the remaining stack operators
have their head-set equal to $\emptyset$, since they do not
refer to non terminals.

The following subsections present the extensions needed to
support typing the new operators.

\subsubsection{Typing rules for the new operators}

We start by defining the typing rules for our new PEG repetition operators.
Figure~\ref{fig:typing-repetition} presents the typing rules for the new
repetition operators. Typing rules make use of the following notations:
1) $\mathbf{norm}^*(i)$ denotes a static analysis
version of the normalization algorithm in which operators \textbf{top.tonat}
and \textbf{top.length} are replaced by 0\footnote{This is necessary because these
operators value depends on the current parser stack state, which is unknown
during type inference.}; 2) we let $\tau^v$ denote the
type $\langle v = 0 \lor \tau.null, \tau.head\rangle$ and
3) notation $\tau[v_1,v_2]$ denote the type
$\langle v_1 = 0 \lor \tau.null, \tau.head\rangle$.


The first type system rule shows that an exact repetition is considered
nullable if either its underlying expression is nullable or its normalized index
is equal to either $0$. The head-set for an exact repetition is
the same of its parameter expression. We also restrict that normalized indexes can
only be numeric values, $n$.
Typing of the interval repetition operator is similar to exact repetition, but
we allow the upper interval bound to be \textbf{infty}.
Intervals are considered nullable when
its parameter expression is nullable or either the normalized left index, $n$,
is equal to zero. We also do not allow intervals that have an \textbf{infty}
upper bound to be nullable, since such parsing expression will diverge
whenever they succeed without consuming any input.

\begin{figure}[H]
  \[
    \begin{array}{c}
      \infer{\Gamma \vdash e^i : \tau^n}
            {\begin{array}{c}
              \mathbf{norm}^*(i) = n\quad \Gamma \vdash e : \tau \\
            \end{array}} \\ \\
        \infer{\Gamma\vdash e[i,j] : \tau[n,v] }
              {\begin{array}{ccc}
                  \Gamma \vdash e : \tau &
                  \mathbf{norm}^*(i) = n &
                  \mathbf{norm}^*(j) = v \\
                  \multicolumn{3}{c}{
                  \neg (v \equiv \mathbf{infty} \land\tau.null)}
               \end{array}}
    \end{array}
  \]
  \caption{Typing rules for new PEG repetition operators}
  \label{fig:typing-repetition}
\end{figure}

Figure~\ref{fig:typing-stack} presents the typing rules for the PEG stack
operators. The first rule specifies that the type for \textbf{push}($e$) is
the same as $e$, since it simply pushes the $e$'s consumed input and its
head-set will be the same as $e's$ head-set. Since all other stack operators
can succeed without consuming input, all of them are considered nullable.
As presented in Figure~\ref{fig:new-head-set}, the head-set of \textbf{pop},
\textbf{drop}, \textbf{peek}, \textbf{peekall} and \textbf{dropall} are the
empty set.

\begin{figure}[H]
  \[
    \begin{array}{cc}
      \infer{\Gamma \vdash \mathbf{push}(e) : \tau}
            {\Gamma \vdash e : \tau}
      &
      \infer{\Gamma \vdash \mathbf{pop} : \langle true, \emptyset\rangle}{}  \\ & \\
      \infer{\Gamma \vdash \mathbf{peek} : \langle true, \emptyset\rangle}{} &
      \infer{\Gamma \vdash \mathbf{peekall} : \langle true, \emptyset\rangle}{} \\ & \\
      \infer{\Gamma \vdash \mathbf{drop} : \langle true, \emptyset\rangle}{} &
      \infer{\Gamma \vdash \mathbf{dropall} : \langle true, \emptyset\rangle}{} \\ & \\
    \end{array}
  \]
  \caption{Typing rules for new PEG stack operators}
  \label{fig:typing-stack}
\end{figure}

\subsection{Extending the type inference algorithm}

In this section we describe the necessary extensions to the type inference algorithm
created by Cardoso et. al.~\cite{Cardoso23} to support the new repetitions and
stack operators.

\paragraph{Extending the constraint syntax} We start by including index normal forms
on the constraint grammar (taken from~\cite{Cardoso23}). The non terminal $v$ denotes
a normal form of an index expression, which can be a numeric value or
constant \textbf{infty}. The complete constraint grammar is as follows:

  \[
    \begin{array}{lcl}
      \tau & \to & \alpha\,\mid\,\langle b, S \rangle\\
      v & \to & n\,\mid\,\mathbf{infty}\\
      \mathbf{t} & \to & A\,\mid\,b\,\mid\,S\,\mid\,\tau\,\mid\,v\\
      C    & \to & \mathbf{true}\,\mid\,\mathbf{false}\,\mid\,\mathbf{t} \equiv \mathbf{t}\,\mid\,C \land C\,\mid\, \exists
                   \alpha.C\\
           & \mid & \mathbf{def}\:A :\tau\:\mathbf{in}\:C
    \end{array}
  \]
Constraints are first-order logic formulas over types and its meaning
can be defined inductively. We omit the constraint semantics, since its
definition is the same presented in~\cite{Cardoso23}.

\paragraph{Extending constraint generation and solving} After including index expressions
in the constraint syntax, we need to include equations for the new operations
in the constraint generation algorithm.
In order to generate constraints for the exact repetition operator, we use
an existential quantifier to define a new variable, $\alpha$, that is passed
as argument to the generation of constraints for $e$. The algorithm creates
an equality between the argument type, $\tau$, and the repetition operator
type over $\alpha$. We also include an inequality to assert that the
normalized index should not be \textbf{infty}.
\[
  \langle\langle e^i : \tau \rangle\rangle = \exists \alpha . \langle\langle e,\alpha\rangle\rangle \land \tau \equiv \alpha^{\mathbf{norm}^*(i)} \land \mathbf{norm}^*(i) \not\equiv \mathbf{infty}\\
\]
Next, we consider the generation of constraints for the interval repetition operator, which
follows the same pattern of the exact repetition: we generate the constraint for sub-expression
$e$, considering an existential quantified type variable and create an equality between the variable
type and the input argument type, $\tau$. We also impose that the interval lower-bound
cannot be \textbf{infty} and that if the upper-bound is \textbf{infty}, the interval
underlying expression cannot be nullable since it could make the parser loop indefinitely.
\[
  \begin{array}{rl}
    \langle\langle e[i,j] : \tau \rangle\rangle =   & \exists \alpha . \langle\langle e, \alpha\rangle\rangle \land \tau \equiv \alpha[\mathbf{norm}^*(i),\mathbf{norm}^*(j)]\\
                                            \land{} & \mathbf{norm}^*(i) \not\equiv\mathbf{infty} \\
                                            \land{} & \neg (\mathbf{norm}^*(j) \equiv \mathbf{infty} \land \alpha.null)\\
  \end{array}
\]
The constraint for the \textbf{push} operator is just the constraint of its argument expression and
all other stack expressions just generate an equality between its input argument type and
$\langle true,\emptyset\rangle$.

\begin{figure}[H]
  \[
    \begin{array}{lcl}
      \langle\langle \mathbf{push}(e) : \tau\rangle\rangle & = & \langle\langle e : \tau \rangle\rangle \\
      \langle\langle \mathbf{pop} : \tau \rangle\rangle & = & \tau \equiv \langle true, \emptyset \rangle\\
      \langle\langle \mathbf{drop} : \tau \rangle\rangle & = & \tau \equiv \langle true, \emptyset \rangle\\
      \langle\langle \mathbf{dropall} : \tau \rangle\rangle & = & \tau \equiv \langle true, \emptyset \rangle\\
      \langle\langle \mathbf{peek} : \tau \rangle\rangle & = & \tau \equiv \langle true, \emptyset \rangle\\
      \langle\langle \mathbf{peekall} : \tau \rangle\rangle & = & \tau \equiv \langle true, \emptyset \rangle\\
    \end{array}
  \]
  \caption{Extending the constraint generation algorithm}
  \label{fig:constraint-gen}
\end{figure}

The inclusion of index expressions do not change the constraint solver specification~\cite{Cardoso23}.
Normalized indexes are just integers or the \textbf{infty} and they only appear as part of the
nullability field of a type, which has its dedicated equation in solver specification, or as part
of an inequality, which can be easily solved by an SMT solver or a simple custom solving algorithm.

Since the proposed changes (the inclusion of indexes and new operators) do not change the
constraint solver, just the constraint generation algorithm, we argue that~\citet{Cardoso23}
soundness and completeness argument for the type inference algorithm
also holds for our Pest-style grammars. This lead to our main result:

\begin{Theorem}[Termination of typed grammars]
  Let $G = (V,\Sigma,R, e_s)$ be a well-typed grammar which may use any of Pest
  repetition or stack operators. Then, for all $w$, exists $r$
  such that $(e_s,w,[])\Rightarrow r$.
\end{Theorem}

The only way of a parsing expression to loop over an input is due to
the presence of left-recusive rules or nullable expressions
under a star operator in a grammar~\cite{Ford2004}. Since the type system (and its
inference algorithm) reject any grammar that have these issues,
any well-typed grammar is guaranteed to terminate.

\section{Examples}~\label{sec:examples}

We implemented a Racket library, called \emph{typed-peg},
which implements the type inference and the semantics of grammars with the proposed new
operators. In this section, we present a complete example grammar and discuss how
our approach avoid the looping behavior reported by Pest users~\cite{PESTBUG}.

% \paragraph{A grammar for the Heartbeat packages} One of the most famous binary data format
% bug is the so-called ``heartbleed''. The ASN.1 binary interface uses a ``heartbeat''
% package to check if the other remote party still alive. A well-formed heartbeat package
% is formed by the following components:
% \begin{itemize}
%   \item A type field, which indicates what kind of ``heartbeat'' is being used,
%   \item A field for the length $l$ --- a two byte value,
%   \item A challenge $C$ of length $|C| = l$, and
%   \item An arbitrary number of padding bytes.
% \end{itemize}
%
% \begin{verbatim}
% #lang typed-peg
% bit <-- '0' / '1'
% byte <-- bit^8
% type <-- byte
% length <-- push(byte^2)
% challenge <-- byte^top.tonat
% padding <-- byte*
% start: type ~ length ~ challenge ~ padding
% \end{verbatim}
% The previous grammar is a simple encoding of the heartbeat package format.
% A key issue regarding implementing parsers for this format is to ensure that
% challenge has the size specified by the length field. Using the stack operator
% \textbf{push} we insert the parsed length into the execution stack and retrieve
% its numeric value using the index expression \textbf{top.tonat}, which parses
% the challenge using the exact repetition operator.
%
\paragraph{A grammar for the PNG format}

A popular image file format is the Portable Network Graphics (PNG), which
is represented by the following pieces of data: 1)
A format signature (8 bytes), which always corresponds to the
following integer values: 137, 80, 78, 71, 13, 10, 26, 10;
2) A 4 bytes natural number $n$, which represents the data length;
3) 4 bytes chunk type value;
4) The image data, a sequence of $n$ bytes and
5) a 4 bytes CRC checksum.
A possible grammar to specify the PNG format is:
\begin{verbatim}
#lang typed-peg
bit <-- '0' / '1'
byte <-- bit^8
fist-four <-- '137' ~ '80' ~ '78' ~ '71'
snd-four <-- '13' ~ '10' ~ '26' ~ '10'
signature <-- first-four ~ snd-four
length <-- push(byte^4)
type <-- byte^4
data <-- byte^top.tonat
crc <-- byte^4
start: signature ~ length ~ type ~ data ~ crc
\end{verbatim}
Notice that the use of an exact repetition operator combined with
index \textbf{top.tonat} allow us to guarantee that the data block
has the correct size of the format length field.

\paragraph{Rejecting reported looping examples} Now, let's turn our
attention to some expressions which Pest users reported as
non terminating.

\begin{verbatim}
forever1 <-- (push('') ~ pop)*
forever2 <-- popall ~ (popall)*
forever3 <-- forever3
\end{verbatim}
In the first two examples, we have nullable expressions under a star
operator. In the first rule, we have a repetition of \textbf{push} operator, which
has an empty string expression as its argument, followed by \textbf{pop}.
A \textbf{push} is nullable only if its argument is nullable. Since it
has an empty string as argument, the expression \verb|push('')|
accepts the empty string. Next, we have a pop operator which is nullable,
by the definition. In this way, the type system rejects \verb|forever1|,
since it has a nullable expression under a star. The second example is also
ill-typed since it has a \textbf{popall}, a nullable operator, as
argument to a PEG repetition.
The third example shows a simple left recursive rule
which is also rejected by type inference algorithm.

% \paragraph{A grammar for an identation sensitive language}
%
% Parsing identation sensitive languages poses a challenge to language developers.
% The next grammar is an example of how stack operators can be used to define
% a language which uses different levels of identation. The idea is to use the
% stack to control the identation level.
% \begin{verbatim}
% #lang typed-peg
% new_line <-- '\n'
% char <-- 'a' / ... / 'z' / '0' / ... / '9'
% ident_level <-- ' '+ / '\t'+
% block_name <-- char+
% block <-- block_name ~ new_line ~ new_block*
% new_block <-- peekall ~
%               push (ident_level) ~
%               block ~
%               (peekall ~ block)* ~
%               drop
% start: new_line* ~ block* ~ new_line*
% \end{verbatim}
% At each block, we start by matching the current block identation
% using the operator \textbf{peekall}. Next, we push the possible
% next identation level and process a possibly empty set of blocks.
% We end a block by removing its identation from the parser stack
% using \textbf{drop}.
%
%

%\paragraph{Discussion}
% Our work uses the framework of operational semantics and type
% theory to provide a solid foundation to Pest stack and repetition
% operators. The main issue pointed by Pest users is failure of
% detecting non termination of grammars. The proposed type inference
% algorithm can avoid this problem by ruling out any grammar which
% has direct or indirect left-recursive rules and nullable expressions
% as a parameter to the Kleene star operator.

% During our formalization, we notice that the inclusion
% of indexes that allow the inspection of the topmost stack item
% would increase the tool expressivity, since they allow concise
% specifications for some context-sensitive formats.
% However, the proposed
% typing algorithm does not avoid some kinds of run-time errors, notably
% errors caused by \textbf{top.tonat} and \textbf{top.length}, since
% the stack is empty or if its top element is not a
% string formed only by digits, in the case of \textbf{top.tonat}.
% Current version of the Pest tool do not provide support to \textbf{top.nat}
% and \textbf{top.length} indexes.

% A limitation of our current prototype is that it does not handle different
% input encodings. The semantics of index expression \textbf{top.tonat} is
% dependent of current input encoding to convert the topmost stack value to
% a natural number, if possible. The proper handling of multiple encodings
% is left for future work.

%While our approach is able to detect non terminating grammars it is not avoid
%some runtime errors, notably caused by \textbf{top.tonat} and \textbf{top.length},
%since if the parser stack is empty (for both operations) or if its top element is
%not a valid number (for \textbf{top.tonat}). Another limitation of our current
%prototype is that it does not handle different input encodings. The semantics of
%the index expression \textbf{top.tonat} depends on the current input encoding to
%covert it into a number, if possible. The proper handling of multiple encodings is
%left for future work.

\section{Related work}~\label{sec:related}

The works of Ribeiro et al. ~\cite{Ribeiro19} and Cardoso et. al.~\cite{Cardoso23}
proposed a type system and a type inference algorithm for PEGs that is sound with
respect to Ford's well-formedness predicate. Authors argue that the use
of a type system provide a more predictable and compositional behavior.
Another work that uses types to ensure parsing termination was proposed by
Krishnaswami et. al.~\cite{Krishnaswami19}. The authors define a type system
for $\mu$-regular expressions, an algebraic presentation of context-free
languages, and proved that their type system only accepts LL(1) grammars.
Our work extend both the type system for PEGs and its inference algorithm to
Pest operators while having the guarantee that no well-typed grammar
will loop over inputs.

The development of formalisms and tools for correct parsing of complex data
formats is subject of some recent works.  Zhang et. al.~\cite{Zhang23} proposed
interval parsing grammars (IPG) as formalism for express some context-sensitive
formats present in binary data like video and image formats. An IPG attaches to
every non terminal/terminal an interval, which specifies the range of input consumed.
By connecting intervals and attributes, the context-sensitive patterns in file formats
can be well handled. Unlike \emph{typed-peg}, IPGs allow the use semantic actions to
perform arbitrary computations over attributes. While such approach provide extra
power, in our view, it clutter since the parser specification is not formed only by
grammar rules but also program pieces to handle parsed data.
%Another work that
%proposes a formalism to deal with data formats was developed by Lucks
%et. al.~\cite{Lucks17}. Authors devised a new class of languages, named
%``calc-regular languages'', which enjoy the property of prefix-freeness that is
%present in several binary data formats. In order to provide concise specification
%of calc-regular languages, author devised \emph{calc-regular expressions}, an
%extension of regular expressions. The parsing of such languages can be made by
%finite state machines with accumulators, which are proved equivalent to calc-regular
%expressions. Zephyr et. al~\cite{Zephyr21} proposed an extension to PEGs, based on
%calc-regular expressions, named calc-PEGs which provide a specific length
%operator to PEGs. Authors discuss a $O(n^2)$ algorithm to parse calc-PEGs and
%present a tool, called pegmatite, that produce parsers from calc-PEGs specifications.
%Unlike our work, authors do not present any termination guarantee of their
%parsers, neither provide a detailed specification of calc-PEGs.

\section{Conclusion}~\label{sec:conclusion}

In this work we proposed a formalization of an extended version of Pest style PEG
grammars. %We provide a formalization of stack manipulation and repetition operators
%for Pest grammars.
Besides a formal definition of the operator semantics, we
extended a typing inference algorithm for PEGs~\cite{Cardoso23} which guarantee
termination of well-typed grammars to handle Pest operators.
As future work, we
intend to continue develop of the \emph{typed-peg} Racket library and to
integrate the ideas present in this work in future versions of the Pest tool.



\section*{ACKNOWLEDGMENTS}
This work is supported by the \grantsponsor{FAPEMIG}{Fundação de Amparo à Pesquisa de Minas Gerais}~
under grant number APQ-01683-21.


% \section*{References}
\bibliographystyle{ACM-Reference-Format}
\bibliography{references}

\end{document}

\end{document}
