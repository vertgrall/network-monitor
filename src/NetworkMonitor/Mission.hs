module NetworkMonitor.Mission
  ( renderMissionPanel
  ) where

import NetworkMonitor.Animate (blockBar, oscilloscope, spinnerAt)
import NetworkMonitor.Apps (AppEmit (..), computeAppEmits, renderAppLines)
import NetworkMonitor.Flow (Flow (..), HostEmit (..), renderFlowPanel)
import NetworkMonitor.Format (formatRate, padR)
import NetworkMonitor.Intel (IntelSnapshot (..), renderIntelLines)

renderMissionPanel ::
  Int ->
  Bool ->
  Double ->
  IntelSnapshot ->
  [HostEmit] ->
  [Flow] ->
  [String] ->
  [String]
renderMissionPanel tick colorOn interval intel emits flows alertLines =
  renderIntelLines intel
    ++ [ "  " ++ spinnerAt tick ++ " MISSION CONTROL snapshot" ]
    ++ alertLines
    ++ take 8 (renderFlowPanel tick colorOn interval (take 6 emits) flows)
    ++ [ ""
       , "  Per-app emitters:"
       ]
    ++ renderAppLines tick colorOn (take 6 (computeAppEmits emits))
    ++ [ ""
       , "  Top emit rate: "
          ++ case emits of
               (e : _) -> formatRate (emitTxRate e)
               _ -> "n/a"
       , ""
       ]
