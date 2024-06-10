*** |  (C) 2006-2023 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de

*** SOF ./modules/30_biomass/magpie/postsolve.gms

p30_demPe(ttot,regi) =
  sum(pe2se(enty,enty2,te)$(sameas(enty,"peoil") OR sameas(enty,"pecoal") OR sameas(enty,"pegas") OR sameas(enty,"pebiolc") OR sameas(enty,"pebios") OR sameas(enty,"pebioil")),
    vm_demPe.l(ttot,regi,enty,enty2,te)
  ) 
  + sum(entySe,
      sum(te,
          vm_prodSe.l(ttot,regi,"pegeo",entySe,te)
        + vm_prodSe.l(ttot,regi,"pehyd",entySe,te)
        + vm_prodSe.l(ttot,regi,"pewin",entySe,te)
        + vm_prodSe.l(ttot,regi,"pesol",entySe,te)
        + vm_prodSe.l(ttot,regi,"peur",entySe,te)
      )
    ) * 100/40  !!! substitution method
;

**** MAgPIE coupling between Nash iterations ***

*DK* Read production of 2nd gen. purpose grown bioenergy from MAgPIE (given to MAgPIE from previous Remind run)
$if %cm_MAgPIE_coupling% == "on"  Execute_Loadpoint 'magpieData.gdx' pm_pebiolc_demandmag_coupling;
$if %cm_MAgPIE_coupling% == "on"  pm_pebiolc_demandmag(ttot,regi) = pm_pebiolc_demandmag_coupling(ttot,regi);

*DK* Read prices and costs for 2nd gen. purpose grown bioenergy from MAgPIE (calculated with demnad from previous Remind run)
$if %cm_MAgPIE_coupling% == "on"  Execute_Loadpoint 'magpieData.gdx' p30_pebiolc_pricemag_coupling;
$if %cm_MAgPIE_coupling% == "on"  p30_pebiolc_pricemag(ttot,regi) = p30_pebiolc_pricemag_coupling(ttot,regi);

display pm_pebiolc_demandmag;

*** EOF ./modules/30_biomass/magpie/postsolve.gms
