
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
\DeclareSIUnit\meter{\metre}
\DeclareSIUnit\mbar{mbar}
\DeclareSIUnit\density{\gram\per\milli\liter}
\DeclareSIUnit\kJpmole{\kilo\joule\per\mole}
\DeclareSIUnit\spc{\siemens\per\centi\meter}
\DeclareSIUnit\M{M}
\DeclareSIUnit\Smolar{\siemens\centi\meter\squared\per\mole}

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
        \textbf{\sffamily Abstract} \\ {The specific heat capacity of ethanol solutions at different concentrations was examined, yielding \qty{2920 \pm 100}{\joule\per\kelvin\per\kilo\gram} for \qty{80.09}{\percent}, \qty{2590 \pm 100}{\joule\per\kelvin\per\kilo\gram} for \qty{88.86}{\percent} and \qty{2430 \pm 100}{\joule\per\kelvin\per\kilo\gram} for \qty{93.73}{\percent}. Combining these values with results from other teams, the entire dilution range of ethanol was surveyed. With the same apparatus, the enthalpy of solution of ammonium nitrate was determined at \qty{29.0 \pm 1.6}{\kilo\joule\per\mole}. The acetate concentration of a vinegar sample was determined by calorimetric titration, i. e. measuring heat produced by neutralisation of acetic acid. The continuous addition of sodium hydroxide yielded \qty{49.74 \pm 0.11}{\gram\per\liter}, while adding fixed doses of \ce{NaOH} indicated a concentration of \qty{50.6 \pm 1.2}{\gram\per\liter} of acetic acid. Finally, the melting enthalpy of ice was examined, returning a value of \qty{330 \pm 50}{\kilo\joule\per\kilo\gram}.} %The specific conductivity $\kappa$ of several water samples was measured and compared. Next, the temperature dependence of a \qty{0.1}{\M} potassium carbonate solution was examined, yielding a thermal coefficient $\alpha_{\mathrm{K_2CO_3}} =$ \qty[per-mode=reciprocal]{0.02143 \pm 0.00008}{\per\kelvin}. The specific conductivity was measured again for \qty{0.01}{\M} solutions of potassium carbonate and sodium carbonate, and the obtained results were subsequently used to calculate the respective molar conductivity, receiving results of $\Lambda_{\mathrm{K_2CO_3}} = $ \qty{216 \pm 4}{\Smolar} and $\Lambda_{\mathrm{Na_2CO_3}} = $ \qty{131.1 \pm 0.6}{\Smolar} respectively. By measuring $\kappa$ of sodium carbonate solutions of varying concentrations, the limiting molar conductivity could be extrapolated at $\Lambda^0_{\mathrm{Na_2CO_3}} = $ \qty{251 \pm 7}{\Smolar}. Via conductometric titration, the unknown concentration of a potassium carbonate solution was found to be \qty{9.2 \pm 0.3}{\milli\M}.}\par
        \rule{\linewidth}{1pt}
        % \qty{80.09}{\percent}, \qty{88.86}{\percent} and \qty{93.73}{\percent} of ethanol respectively (mass percent). Specific heat capacities of \qty{2920 \pm 100}{\joule\per\kelvin\per\kilo\gram}, \qty{2590 \pm 100}{\joule\per\kelvin\per\kilo\gram} and \qty{2430 \pm 100}{\joule\per\kelvin\per\kilo\gram} were found.
    \end{center}
    \vfill
    \begin{center}
        %\begin{adjustbox}{width=1\textwidth}
            \begin{tabular}{ccc}
                 %Zurich, \today \\
                 \multicolumn{2}{c}{Zurich, February 4, 2024} \\
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
