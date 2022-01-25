final: prev: with final; {


  haskellPackages = prev.haskellPackages.override (old: {
    overrides = lib.composeManyExtensions [
                  (old.overrides or (_: _: {}))
                  (self: super: {
                    formatting = self.formatting_7_1_2;
                  })
                  (haskell.lib.packageSourceOverrides { hid = ./.; })
                ];
  });

  hid = haskell.lib.justStaticExecutables haskellPackages.hid;

  ghcWithHid = haskellPackages.ghcWithPackages (p: [ p.hid ]);

  ghcWithHidAndPackages = select :
    haskellPackages.ghcWithPackages (p: ([ p.hid ] ++ select p));


  jupyterlab = mkJupyterlab {
    haskellKernelName = "Hid";
    haskellPackages = p: with p;
      [ # add haskell pacakges if necessary
        hid
        hvega
        ihaskell-hvega
      ];
    pythonKernelName = "Hid";
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
