
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{report}[2020/12/31 report for pc-practical course]

\LoadClass[11pt]{article}

% basic
\usepackage[
    a4paper,
    left = 3cm,
    right = 3cm,
    top = 3cm,
    bottom = 2cm,
    bindingoffset = 0mm,
    footskip = 1.5cm,
    includefoot
]{geometry}
\usepackage{graphicx}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{hyperref}
\usepackage{float}
\usepackage{pdfpages}
\usepackage{upgreek}
\usepackage[justification=centering]{caption}
\usepackage{subfigure}
\usepackage{xcolor}

\definecolor{bg}{RGB}{230,230,230}

\usepackage{siunitx}
\sisetup{locale = DE, output-decimal-marker = {.}, per-mode = symbol-or-fraction, round-mode=none, round-minimum=0, round-pad=false, separate-uncertainty = true,multi-part-units=single}

\DeclareSIUnit\atm{atm}
\DeclareSIUnit\meter{m}
\DeclareSIUnit\mbar{mbar}
\DeclareSIUnit\density{\gram\per\milli\liter}
\DeclareSIUnit\kJpmole{\kilo\joule\per\mole}

% style
\usepackage{fancyhdr}
\usepackage{multicol}
\usepackage{adjustbox}

% \usepackage{listingsutf8}


\usepackage{minted}
\usemintedstyle{colorful}
\setminted[R]{bgcolor=bg, breaklines=true, breakafter=-/}
\newmintedfile{R}{}

\usepackage{markdown}
\markdownSetup{pipeTables,tableCaptions}


\usepackage{color}
\usepackage[activate={true,nocompatibility},final,tracking=true,kerning=true,spacing=false,factor=1100,stretch=10,shrink=10]{microtype}
% activate={true,nocompatibility} - activate protrusion and expansion
% final - enable microtype; use "draft" to disable
% tracking=true, kerning=true, spacing=true - activate these techniques
% factor=1100 - add 10% to the protrusion amount (default is 1000)
% stretch=10, shrink=10 - reduce stretchability/shrinkability (default is 20/20)

% chemical

\usepackage{ghsystem}

\usepackage{chemmacros}
\sisetup{
    round-mode = places,
    round-precision=3
}

\usepackage{modiagram}

%Tables
\usepackage{tabularray}
\UseTblrLibrary{siunitx}

\usepackage{longtable}
\usepackage{ghsystem}
\usepackage{chemmacros}
\chemsetup{
  formula = mhchem ,
  %modules= all,
  modules = {redox,reactions}
}
\chemsetup[redox]{pos=top}
\chemsetup[redox]{explicit-sign = true}
\sisetup{
    round-mode = places,
    round-precision=3
}

%Tables
\usepackage{tabularray}
\usepackage{xcolor}

%Bibliography
\usepackage{biblatex}
\addbibresource{References.bib}

% config
\graphicspath{{figures/}}
\raggedcolumns

\captionsetup[figure]{
	labelfont={bf},
	labelformat={default},
	labelsep=colon,
	name={Figure}
}

% (Re)defining Commands for maketitle
\renewcommand{\title}[1]{
  \newcommand{\showingtext}{#1}
}
\newcommand{\authors}[9]{
  \newcommand{\authone}{#1}
  \newcommand{\authonemail}{#2}
  \newcommand{\authonedep}{#3}
  \newcommand{\authtwo}{#4}
  \newcommand{\authtwomail}{#5}
  \newcommand{\authtwodep}{#6}
  % activate forthird author
  %\newcommand{\auththree}{#7}
  %\newcommand{\auththreemail}{#8}
  %\newcommand{\auththreedep}{#9}
}
\newcommand{\assistant}[2]{
  \newcommand{\assname}{#1}
  \newcommand{\assmail}{#2}
}
\renewcommand{\abstract}{
    \newcommand{\abstracttext}
}
\renewcommand{\maketitle}{
    \begin{center}
    \bfseries\LARGE\sffamily\showingtext
    \end{center}\par\bigskip
    \begin{center}
        \begin{tabular}{ccc}
            \authone\par\smallskip & \authtwo\par\smallskip \\
            \authonedep\par\smallskip & \authtwodep\par\smallskip \\
            \href{mailto:\authonemail}{\authonemail}\par\bigskip & \href{mailto:\authtwomail}{\authtwomail}\par\bigskip
            
            
            
        \end{tabular} \\
        
        %\auththree\par\smallskip
        %\auththreedep\par\smallskip
        %\href{mailto:\auththreemail}{\auththreemail}\par\bigskip
        Assistant:\par\smallskip
        \assname\par\smallskip
        \href{mailto:\assmail}{\assmail}
    \end{center}
    \begin{center}
        \rule{\linewidth}{1pt}
        \textbf{\sffamily Abstract} \\ {The molar enthalpy and entropy of evaporation of acetone and n-hexane were determined via two experiments. First, by measuring vapor pressure and boiling temperature between \qtyrange{100}{1000}{\mbar} and \qtyrange{15}{55}{\celsius}. By plotting the logarithmic pressure against the inverse temperature, thus linearizing the correlation, the molar enthalpy of evaporation $\Delta_VH$ was found using the Clausius-Clapeyron equation. Acetone was found to have $\Delta_VH=$ \qty{32.49 \pm 0.26}{\kJpmole} and n-hexane $\Delta_VH=$ \qty{32.66 \pm 0.25}{\kJpmole}. With the Gibbs-Helmholtz equation, entropies of evaporation of $\Delta_VS(T_0)$ = \qty{98.62}{\joule\per\mole\per\kelvin} for acetone and $\Delta_VS(T_0)$ = \qty{95.74}{\joule\per\mole\per\kelvin} for n-hexane were determined for standard boiling temperature at 1 atm. In the second experiment, the enthalpy of evaporation was determined measuring the heat drawn from a surface during evaporation. With a comparative measurement against methanol, the calculations for acetone, $\Delta_VH=$\qty{30.2 \pm 2.8}{\kJpmole}, and for n-hexane, $\Delta_VH=$ \qty{32.0 \pm 1.9}{\kJpmole}, confirmed the previous results. To ensure the purity of the used chemicals, their specific density was verified via two different methods in addition to measurements of their respective refractive indexes.}\par
        \rule{\linewidth}{1pt}
    \end{center}
    \vfill
    \begin{center}
        %\begin{adjustbox}{width=1\textwidth}
            \begin{tabular}{ccc}
                 %Zurich, \today \\
                 \multicolumn{2}{c}{Zurich, January 12, 2024} \\
                \includegraphics[width=.2\textwidth]{figures/sign_janosch.jpg} & \includegraphics[width=.4\textwidth]{figures/sign_maria.pdf}\\
               \authone & \authtwo
            \end{tabular}
        %\end{adjustbox}
    \end{center}
}

% header/footer settings
\fancypagestyle{report}{%
  \renewcommand{\headrulewidth}{0pt}
  \fancyhf{}
  \fancyhead[L]{Physikalisch-chemisches Praktikum\\ ETH Zürich, HS23}
  \fancyfoot[C]{\thepage}
}
\pagestyle{report}

% code highlighting modifications (imported from wikipedia and then modified so it fits)
\definecolor{mygreen}{rgb}{0.4,0.8,0.4}
\definecolor{mygray}{rgb}{0.5,0.5,0.5}
\definecolor{mymauve}{rgb}{0.68,0.4,0.92}
\definecolor{myblue}{rgb}{0.6,0.6,1.0}
