module Editor.Model where

type Brush =
      Elevation | River

type alias Editor =
    { brush:Brush }