final: prev: with final; {


  haskellPackages = prev.haskellPackages.override (old: {
    overrides = lib.composeManyExtensions (with haskell.lib; [
                  (old.overrides or (_: _: {}))
                  (self: super: {
                    # formatting = self.formatting_7_1_2;
                    colonnade = doJailbreak (markUnbroken super.colonnade);
                    streaming-utils = doJailbreak (markUnbroken super.streaming-utils);
                  })
                  (packageSourceOverrides { hid-examples = ./.; })
                ]);
  });

  hid-examples = haskell.lib.justStaticExecutables haskellPackages.hid-examples;

  ghcWithhid-examples = haskellPackages.ghcWithPackages (p: [ p.hid-examples ]);

  ghcWithhid-examplesAndPackages = select :
    haskellPackages.ghcWithPackages (p: ([ p.hid-examples ] ++ select p));


  jupyterlab = mkJupyterlab {
    haskellKernelName = "hid-examples";
    haskellPackages = p: with p;
      [ # add haskell pacakges if necessary
        hid-examples
        hvega
        ihaskell-hvega
      ];
    pythonKernelName = "hid-examples";
    pythonPackages = p: with p;
      [ # add python pacakges if necessary
        scipy
        numpy
        tensorflow-bin
        matplotlib
        scikit-learn
        pandas
        lightgbm
      ];
  };
}
