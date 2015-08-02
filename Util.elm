module Util where

import Debug

import Math.Vector3 exposing (Vec3, toTuple, fromTuple)
import Math.Matrix4 exposing (Mat4)

import WebGL exposing (Shader)

type alias Vertex = { position : Vec3, color : Vec3 }

debugVec: String -> Vec3 -> Vec3
debugVec name vec =
  let (x, y, z) = toTuple vec
  in Debug.watch name (x, y, z) |> fromTuple

  -- Shaders

vertexShader :
  Shader
  { attr | position:Vec3, color:Vec3 }
  { unif | perspective:Mat4, localPos:Vec3, worldPos:Vec3, rotation:Float }
  { vcolor:Vec3 }
vertexShader = [glsl|

attribute vec3 position;
attribute vec3 color;
uniform mat4 perspective;
uniform vec3 worldPos;
uniform vec3 localPos;
uniform float rotation;
varying vec3 vcolor;

void main () {
    mat3 rotMat;
    rotMat[0] = vec3(cos(rotation), sin(rotation), 0);
    rotMat[1] = vec3(-sin(rotation), cos(rotation), 0);
    rotMat[2] = vec3(0, 0, 1);
    vec3 l = rotMat * localPos;
    gl_Position = perspective * vec4(l + worldPos + position, 1.0);
    vcolor = color;
}

|]


fragmentShader : Shader {} u { vcolor:Vec3 }
fragmentShader = [glsl|

precision mediump float;
varying vec3 vcolor;

void main () {
    gl_FragColor = vec4(vcolor, 1.0);
}

|]
