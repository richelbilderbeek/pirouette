    **Compare with**  **TB**   **WG**   **WB**
  ------------------ -------- -------- --------
              **TG**    Y        Y        N
              **TB**    .        N        Y
              **WG**    .        .        Y
              **WB**    .        .        .

  :  Error distributions that are relevant to compare. TG: true tree,
  generative model. TB: true tree, best candidate model. WG: twin tree,
  generative model. WB: twin tree, best candidate model.
  []{data-label="tab:relevant_comparisions"}

          **Condition**  **Expectation**  **Interpretation**
  --------------------- ----------------- ---------------------------------------------------------------------------------------------------------
          **$TG > TB$**    Unexpected     Novel tree prior is more related to best candidate than hand-picked tree prior
    **$TG \approx TB$**     Possible      Hand-picked tree prior is just as suitable as the best candidate tree prior
          **$TG < TB$**     Expected      Hand-picked tree prior is the most related tree prior
          **$TG > WG$**      Unknown      Novel tree prior important
    **$TG \approx WG$**      Unknown      Novel tree prior unimportant
          **$TG < WG$**    Unexpected     Twinning procedure increases inference errors when using hand-picked tree prior
          **$TB > WB$**     Expected      Impact of novel tree prior cannot be compensated for by model selection: twin tree with low likelihood?
    **$TB \approx WB$**     Possible      Best candidate tree priors perform equally well in true and twin tree: true and twin tree similar?
          **$TB < WB$**    Unexpected     Twinning procedure increases inference errors when using best tree prior candidate
          **$WG > WB$**    Unexpected     Hand-picked tree prior (that equals the twinning tree prior!) worse than best candidate tree prior
    **$WG \approx WB$**     Possible      Twin tree fits equally well to the hand-picked and best candidate tree prior
          **$WG < WB$**     Expected      Hand-pick tree prior (that equals the twinning tree prior) performs as expected

  :  Interpretation of the different error distributions. This assumes
  that the operators ($<$, $\approx$ and $>$) to compare distributions
  are defined. TG: true tree, generative model. TB: true tree, best
  candidate model. WG: true tree, generative model. WB: true tree, best
  candidate model. []{data-label="tab:interpretation"}


