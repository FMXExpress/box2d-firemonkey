{If in your application there are more than one GLCanvas, define this.
 If MultiCanvases is defined, before every process GLCanvas will check
 if self is activated. }
{.$DEFINE MultiCanvases}

// BDS 2010 (BDS 7.0)
{$IFDEF VER210}
  {$DEFINE INLINE_AVAIL}
{$ENDIF}

// BDS 2009 (BDS 6.0)
{$IFDEF VER200}
  {$DEFINE INLINE_AVAIL}
{$ENDIF}

// BDS 2007 (BDS 5.0)
{$IFDEF VER190}
  {$DEFINE INLINE_AVAIL}
{$ENDIF}

// BDS 2006
{$IFDEF VER180}
  {$DEFINE INLINE_AVAIL}
{$ENDIF}

{$IFDEF VER170}
  {$DEFINE INLINE_AVAIL}
{$ENDIF}

{$IFNDEF OP_OVERLOAD}
   {$IFNDEF VER170}
      {$IFNDEF VER160}
         {$IFNDEF VER150}
            {$IFNDEF VER140}
               {$IFNDEF VER130}
                  {$IFNDEF VER125}
                     {$IFNDEF VER120}
                        {$IFNDEF VER110}
                           {$IFNDEF VER100}
                              {$IFNDEF VER93}
                                 {$IFNDEF VER90}
                                    {$IFNDEF VER80}
                                       // Must be using a Delphi newer than D2010
                                       {$DEFINE INLINE_AVAIL}
                                    {$ENDIF}
                                 {$ENDIF}
                              {$ENDIF}
                           {$ENDIF}
                        {$ENDIF}
                     {$ENDIF}
                  {$ENDIF}
               {$ENDIF}
            {$ENDIF}
         {$ENDIF}
      {$ENDIF}
   {$ENDIF}
{$ENDIF}




