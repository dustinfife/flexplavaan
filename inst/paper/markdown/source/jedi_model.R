\begin{center}
\begin{figure}
\usetikzlibrary{arrows}
\begin{tikzpicture}[node distance=2cm, midway, scale=2, every node/.style={scale=2}]

\node[draw, circle] (force) {Force};
\node[draw, circle, node distance = 2cm] (jedi) [right of=force] {Jedi};

\node[draw, rectangle] (saber) [left of=force] {Saber};
\node[draw, rectangle, node distance = 1cm] (fit) [above of=saber] {Fitness};
\node[draw, rectangle, node distance = 1cm] (midi) [below of=saber] {Midichlorian};

\node[draw, rectangle] (hist) [below of=jedi] {History};

\node[draw, rectangle] (e2) [right of=jedi] {Exam II};
\node[draw, rectangle, node distance = 1cm] (e1) [above of=e2] {Exam I};
\node[draw, rectangle, node distance = 1cm] (e3) [below of=e2] {Exam III};

\draw[->] (force) to node [fill=white]{\scriptsize $b$} (saber);
\draw[->] (force) to node [fill=white]{\scriptsize $a$} (fit);
\draw[->] (force) to node [fill=white]{\scriptsize $c$} (midi);

\draw[->] (jedi) to node [fill=white]{\scriptsize $d$} (e1);
\draw[->] (jedi) to node [fill=white]{\scriptsize $e$} (e2);
\draw[->] (jedi) to node [fill=white]{\scriptsize $f$} (e3);

\draw[->] (jedi) to node [fill=white]{\scriptsize $g_1$} (hist);
\draw[->] (force) to node [fill=white]{\scriptsize $g_2$} (hist);

\draw[->] (force) edge[out=25, in=180-25,->, below] node {\scriptsize $\beta$} (jedi);
\end{tikzpicture}
\caption{Simulated dataset with crossloadings on the 'history' indicator. This is the data-generating model.}
\end{figure}
\end{center}