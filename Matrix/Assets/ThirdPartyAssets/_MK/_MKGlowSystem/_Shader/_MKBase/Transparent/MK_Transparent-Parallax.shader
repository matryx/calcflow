Shader "MK/MKGlow/Transparent/Parallax" {
Properties {
	_Color ("Main Color", Color) = (1,1,1,0.25)
	_Parallax ("Height", Range (0.005, 0.08)) = 0.02
	_MainTex ("Base (RGB) Trans (A)", 2D) = "white" {}
	_BumpMap ("Normalmap", 2D) = "bump" {}
	_ParallaxMap ("Heightmap (A)", 2D) = "black" {}
	
	_MKGlowColor("Glow Color", Color) = (1, 1, 1, 0.5)
	_MKGlowPower("Glow Power", Range(0.0, 5.0)) = 2.5
	_MKGlowTex("Glow Texture", 2D) = "black" {}
	_MKGlowTexColor("Glow Texture Color", Color) = (1, 1, 1, 0.25)
	_MKGlowTexStrength("Glow Texture Strength ", Range(0.0, 1.0)) = 1.0
	_MKGlowOffSet("Glow Width ", Range(0.0, 0.075)) = 0.0
}

SubShader {
	Tags {"Queue"="Transparent" "IgnoreProjector"="True" "RenderType"="MKGlow"}
	LOD 500
	
CGPROGRAM
#pragma surface surf Lambert alpha

sampler2D _MainTex;
sampler2D _BumpMap;
sampler2D _ParallaxMap;
fixed4 _Color;
float _Parallax;

sampler2D _MKGlowTex;
half _MKGlowTexStrength;
fixed4 _MKGlowTexColor;

struct Input {
	float2 uv_MainTex;
			float2 uv_MKGlowTex;
	float2 uv_BumpMap;
	float3 viewDir;
};

void surf (Input IN, inout SurfaceOutput o) {
	half h = tex2D (_ParallaxMap, IN.uv_BumpMap).w;
	float2 offset = ParallaxOffset (h, _Parallax, IN.viewDir);
	IN.uv_MainTex += offset;
	IN.uv_BumpMap += offset;
	
	fixed4 c = tex2D(_MainTex, IN.uv_MainTex) * _Color;
	fixed4 d = tex2D(_MKGlowTex, IN.uv_MKGlowTex) * _MKGlowTexColor;
	c += (d * _MKGlowTexStrength);
	o.Albedo = c.rgb;
	o.Alpha = c.a;
	o.Normal = UnpackNormal(tex2D(_BumpMap, IN.uv_BumpMap));
}
ENDCG
}

FallBack "Transparent/Bumped Diffuse"
}
