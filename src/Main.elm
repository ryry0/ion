module Main exposing (main)

import Browser exposing (Document)
import Browser.Events
import Html exposing (a, button, div, input, option, select, text)
import Html.Attributes exposing (class, height, href, style, width)
import Html.Events exposing (onClick, onInput)
import Math.Matrix4 exposing (..)
import Math.Vector2 exposing (..)
import Math.Vector3 exposing (..)
import WebGL exposing (..)


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Vertex =
    { a_position : Vec3
    , a_time : Float
    }


type alias Attractor =
    Float -> Float -> Float -> ( Float, Float, Float )


attractorlist : List String
attractorlist =
    [ "Thomas"
    , "Lorenz"
    , "Aizawa"
    , "Anishchenko"
    , "Bouali"
    , "Coullet"
    , "Yu"
    ]


type alias Model =
    { renderable : List Vertex
    , rotation : Mat4
    , perspective : Mat4
    , resolution : ( Int, Int )
    , attractor : Attractor
    , scale : Float

    --, initial_point : (Float, Float, Float)
    }


windowSizeToResolution : Int -> Int -> Msg
windowSizeToResolution width height =
    Resolution ( width, height )



-- Init


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { renderable = [ initialpoint ]
            , rotation = Math.Matrix4.identity
            , perspective = perspective 1 1
            , resolution = ( 1000, 1000 )
            , attractor = thomas
            , scale = 0.08
            }
    in
    ( model, Cmd.none )


init_scale : Float
init_scale =
    0.08



--0.01 for lorenz
--0.20 for aizawa
--0.05 for ashchch
--0.03 for bouali
--0.08 for thomas
--0.1 for coullet
--0.008 for yu_wang


num_points : Int
num_points =
    8000


perspective : Float -> Float -> Mat4
perspective winx winy =
    mul (makePerspective 45 (winx / winy) 0.01 100)
        (makeLookAt (vec3 0 0 1) (vec3 0 0 0) (vec3 0 1 0))


scaling : Float -> Mat4
scaling scale =
    Math.Matrix4.scale (vec3 scale scale scale) Math.Matrix4.identity


genrotate : Float -> Mat4
genrotate angle =
    rotate angle (vec3 0 1 0) Math.Matrix4.identity


genpoint : Attractor -> Maybe Vertex -> Vertex
genpoint attractor maybevertex =
    case maybevertex of
        Nothing ->
            initialpoint

        Just vertex ->
            let
                x =
                    Math.Vector3.getX vertex.a_position

                y =
                    Math.Vector3.getY vertex.a_position

                z =
                    Math.Vector3.getZ vertex.a_position
            in
            let
                ( newx, newy, newz ) =
                    attractor x y z
            in
            { a_position = vec3 newx newy newz, a_time = vertex.a_time + 0.1 }


initialpoint : Vertex
initialpoint =
    { a_position = vec3 -1.1 1.1 1.1
    , a_time = 0.0
    }



-- -1.1 0 0 for aizawa
-- -1.1 1.0 0.0 for bouali
-- -1.1 1.1 1.1 for thomas
-- 0.1 0.1 0.1 for coullet
-- 1.1 1.1 1.1 for yu_wang


genvertex : Float -> Float -> Float -> Vertex
genvertex x y z =
    { a_position = vec3 x y z
    , a_time = 0.0
    }


queue : List Vertex -> List Vertex
queue list =
    if List.length list > num_points then
        List.take num_points list

    else
        list


lorenz : Attractor
lorenz x y z =
    let
        p =
            10.0

        r =
            28.0

        b =
            8.0 / 3.0

        dt =
            0.01

        dx =
            p * (y - x)

        dy =
            (r * x) - y - (x * z)

        dz =
            (x * y) - (b * z)

        newx =
            x + dx * dt

        newy =
            y + dy * dt

        newz =
            z + dz * dt
    in
    ( newx, newy, newz )


aizawa : Attractor
aizawa x y z =
    let
        alpha =
            0.95

        beta =
            0.7

        gamma =
            0.6

        delta =
            3.5

        zeta =
            0.1

        epsilon =
            0.25

        dt =
            0.01

        dx =
            (z - beta) * x - delta * y

        dy =
            delta * x + ((z - beta) * y)

        dz =
            gamma
                + alpha
                * z
                - ((z ^ 3.0) / 3.0)
                - (x ^ 2 + y ^ 2)
                * (1 + epsilon * z)
                + zeta
                * z
                * x
                ^ 3

        newx =
            x + dx * dt

        newy =
            y + dy * dt

        newz =
            z + dz * dt
    in
    ( newx, newy, newz )


anishchenko_astakhov : Attractor
anishchenko_astakhov x y z =
    let
        i n =
            if n > 0 then
                1

            else
                0

        mu =
            1.2

        eta =
            0.5

        dt =
            0.01

        dx =
            mu * x + y - x * z

        dy =
            -x

        dz =
            -eta * z + eta * i x * (x ^ 2)

        newx =
            x + dx * dt

        newy =
            y + dy * dt

        newz =
            z + dz * dt
    in
    ( newx, newy, newz )


bouali : Attractor
bouali x y z =
    let
        alpha =
            0.3

        zeta =
            1.0

        dt =
            0.01

        dx =
            x * (4 - y) + alpha * z

        dy =
            -y * (1 - x ^ 2)

        dz =
            -x * (1.5 - zeta * z) - 0.05 * z

        newx =
            x + dx * dt

        newy =
            y + dy * dt

        newz =
            z + dz * dt
    in
    ( newx, newy, newz )


thomas : Attractor
thomas x y z =
    let
        beta =
            0.19

        dt =
            0.1

        dx =
            sin y - beta * x

        dy =
            sin z - beta * y

        dz =
            sin x - beta * z

        newx =
            x + dx * dt

        newy =
            y + dy * dt

        newz =
            z + dz * dt
    in
    ( newx, newy, newz )



{--
halvorsen : Attractor
halvorsen x y z =
  let
    alpha = 1.4
    dt = 0.01
    dx = -alpha*x - 4*y - 4*z - y^2
    dy = -alpha*y - 4*z - 4*x - z^2
    dz = -alpha*z - 4*x - 4*y - x^2
    newx = x + dx*dt
    newy = y + dy*dt
    newz = z + dz*dt in
    ( newx, newy, newz )
    --}


coullet : Attractor
coullet x y z =
    let
        alpha =
            0.8

        beta =
            -1.1

        gamma =
            -0.45

        delta =
            -1.0

        dt =
            0.01

        dx =
            y

        dy =
            z

        dz =
            alpha * x + beta * y + gamma * z + delta * x ^ 3

        newx =
            x + dx * dt

        newy =
            y + dy * dt

        newz =
            z + dz * dt
    in
    ( newx, newy, newz )


yu_wang : Attractor
yu_wang x y z =
    let
        alpha =
            10

        beta =
            40

        gamma =
            2

        delta =
            2.5

        dt =
            0.001

        dx =
            alpha * (y - x)

        dy =
            beta * x - gamma * x * z

        dz =
            e ^ (x * y) - delta * z

        newx =
            x + dx * dt

        newy =
            y + dy * dt

        newz =
            z + dz * dt
    in
    ( newx, newy, newz )



-- Update


type Msg
    = Reset
    | Select String
    | Resolution ( Int, Int )
    | DeltaTime Float


attractorOption : String -> Html.Html Msg
attractorOption item =
    option [ Html.Attributes.value item ] [ text item ]


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Select attractor ->
            let
                finalModel =
                    case attractor of
                        "Thomas" ->
                            { model
                                | renderable = [ initialpoint ]
                                , scale = 0.08
                                , attractor = thomas
                            }

                        "Lorenz" ->
                            { model
                                | renderable = [ genvertex -1.1 0 0 ]
                                , scale = 0.01
                                , attractor = lorenz
                            }

                        "Aizawa" ->
                            { model
                                | renderable = [ genvertex -1.1 0 0 ]
                                , scale = 0.2
                                , attractor = aizawa
                            }

                        "Anishchenko" ->
                            { model
                                | renderable = [ genvertex 0.1 0.0 0.0 ]
                                , scale = 0.05
                                , attractor = anishchenko_astakhov
                            }

                        "Bouali" ->
                            { model
                                | renderable = [ genvertex -1.1 1.0 0.0 ]
                                , scale = 0.03
                                , attractor = bouali
                            }

                        "Coullet" ->
                            { model
                                | renderable = [ genvertex 0.1 0.1 0.1 ]
                                , scale = 0.1
                                , attractor = coullet
                            }

                        "Yu" ->
                            { model
                                | renderable = [ genvertex 1.1 1.1 1.1 ]
                                , scale = 0.008
                                , attractor = yu_wang
                            }

                        _ ->
                            { model
                                | renderable = [ initialpoint ]
                                , scale = 0.08
                                , attractor = thomas
                            }
            in
            ( finalModel, Cmd.none )

        Reset ->
            let
                finalModel =
                    { model
                        | renderable = [ initialpoint ]
                    }
            in
            ( finalModel, Cmd.none )

        Resolution ( x, y ) ->
            let
                finalModel =
                    { model
                        | perspective = perspective (toFloat x) (toFloat y)
                        , resolution = ( x, y )
                    }
            in
            ( finalModel, Cmd.none )

        DeltaTime dt ->
            let
                finalModel =
                    { model
                        | renderable = queue <| genpoint model.attractor (List.head model.renderable) :: model.renderable
                        , rotation = mul model.rotation <| genrotate 0.001
                    }
            in
            ( finalModel, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta DeltaTime
        --, Browser.Events.onResize windowSizeToResolution
        ]



-- View


glview : Model -> Html.Html Msg
glview model =
    WebGL.toHtml
        [ width (Tuple.first model.resolution)
        , height (Tuple.second model.resolution)
        , style "display" "block"
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            (WebGL.lineStrip model.renderable)
            { perspective = model.perspective
            , rotation = model.rotation
            , scaling = scaling model.scale
            , resolution =
                vec2 (toFloat <| Tuple.first model.resolution)
                    (toFloat <| Tuple.second model.resolution)
            }
        ]


view : Model -> Document Msg
view model =
    { title = "Ryan Reyes"
    , body = body_html model
    }


body_html : Model -> List (Html.Html Msg)
body_html model =
    [ div [ class "all-container" ]
        [
          div [] [ text "RYAN REYES" ]
          , div [ class "top-container" ]
            [ div [] [ a [ href "https://github.com/ryry0" ] [ text "GITHUB" ] ]
            , div []
                [ a
                    [ href "https://raw.githubusercontent.com/ryry0/LaTeX-Resume/master/reyes-online.pdf" ]
                    [ text "RÉSUMÉ" ]
                ]
            ]
        , div [ class "canvas-container" ]
            [ --select [ onInput Select ] (List.map attractorOption
            --attractorlist) , button [ onClick Reset ] [ text "Reset" ],
              a [ href "https://recu.rs/Doodles" ] [ glview model ]
            ]
        ]
    ]



-- Shaders


vertexShader : Shader { attr | a_position : Vec3, a_time : Float } { unif | perspective : Mat4, rotation : Mat4, scaling : Mat4 } { time : Float }
vertexShader =
    [glsl|

attribute vec3 a_position;
attribute float a_time;
varying float time;

uniform mat4 perspective;
uniform mat4 scaling;
uniform mat4 rotation;

void main () {
  gl_Position = perspective * scaling * rotation * vec4(a_position, 1.0);
  time = a_time;
}

|]

--Speed: change t*0.05 (turns per second).
--Start hue: change 0.6111 (0..1 turns; 0=red, ~0.33=green, ~0.66=blue).
--Brightness: raise/lower L (0..1).
--Color strength: raise/lower C (stay modest, e.g. 0.05–0.12, to remain in sRGB gamut without clipping).

fragmentShader : Shader {} u { time : Float }
fragmentShader =
    [glsl|

precision mediump float;
varying float time;  // expect time in milliseconds (like your original)

//
// OKLab/OKLCh utilities (Björn Ottosson's matrices)
//

vec3 oklab_to_linear_srgb(vec3 lab) {
    float L = lab.x, a = lab.y, b = lab.z;

    float l_ = L + 0.3963377774*a + 0.2158037573*b;
    float m_ = L - 0.1055613458*a - 0.0638541728*b;
    float s_ = L - 0.0894841775*a - 1.2914855480*b;

    float l = l_*l_*l_;
    float m = m_*m_*m_;
    float s = s_*s_*s_;

    return vec3(
        +4.0767416621*l - 3.3077115913*m + 0.2309699292*s,
        -1.2684380046*l + 2.6097574011*m - 0.3413193965*s,
        -0.0041960863*l - 0.7034186147*m + 1.7076147010*s
    );
}

// linear -> sRGB (gamma encode)
float lin2srgb(float c) {
    return (c <= 0.0031308) ? (12.92*c) : (1.055*pow(c, 1.0/2.4) - 0.055);
}
vec3 lin2srgb(vec3 c) { return vec3(lin2srgb(c.r), lin2srgb(c.g), lin2srgb(c.b)); }

// OKLCh -> sRGB
vec3 oklch_to_srgb(float L, float C, float h_rad) {
    float a = C * cos(h_rad);
    float b = C * sin(h_rad);
    vec3 lab = vec3(L, a, b);
    vec3 lin = oklab_to_linear_srgb(lab);
    return lin2srgb(lin);
}

void main() {
    // animate hue
    float t = time * 0.001;              // seconds
    float hueTurns = fract(0.6111 + t*0.5); // start ~220° (light cyan), 0.05 turns/sec
    float hueRad = hueTurns * 6.28318530718;

    // pick a light, in-gamut sweep (adjust if you see clipping)
    float L = 0.80;   // 0.83 perceptual lightness (0..1)
    float C = 0.08;   // chroma (reduce if you notice clipping/banding)

    vec3 rgb = oklch_to_srgb(L, C, hueRad);

    // basic clamp for safety; if this clips, lower C or tweak L
    rgb = clamp(rgb, 0.0, 1.0);

    gl_FragColor = vec4(rgb, 1.0);
}


|]
