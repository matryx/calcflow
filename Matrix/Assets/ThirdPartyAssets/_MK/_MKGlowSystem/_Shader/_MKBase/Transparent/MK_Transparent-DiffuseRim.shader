Shader "MK/MKGlow/Transparent/DiffuseRim" {
Properties {
	_Color ("Main Color", Color) = (1,1,1,0.25)
	_MainTex ("Base (RGB) Trans (A)", 2D) = "white" {}
	
	_RimColor("Rim Color", Color) = (1,1,1,1)
	_RimPower("Rim Power", Range(0.5,6.0)) = 3.0
	
	_MKGlowColor("Glow Color", Color) = (1, 1, 1, 0.5)
	_MKGlowPower("Glow Power", Range(0.0, 5.0)) = 2.5
	_MKGlowTex("Glow Texture", 2D) = "black" {}
	_MKGlowTexColor("Glow Texture Color", Color) = (1, 1, 1, 0.25)
	_MKGlowTexStrength("Glow Texture Strength ", Range(0.0, 1.0)) = 1.0
	_MKGlowOffSet("Glow Width ", Range(0.0, 0.075)) = 0.0
}

SubShader {
	Tags {"Queue"="Transparent" "IgnoreProjector"="True" "RenderType"="MKGlow"}
	LOD 200

CGPROGRAM
#pragma surface surf Lambert alpha

sampler2D _MainTex;
fixed4 _Color;
fixed4 _RimColor;
fixed _RimPower;

sampler2D _MKGlowTex;
half _MKGlowTexStrength;
fixed4 _MKGlowTexColor;

struct Input {
	float4 color : Color;
	float2 uv_MainTex;
			float2 uv_MKGlowTex;
	float3 viewDir;
};

void surf (Input IN, inout SurfaceOutput o) {
	fixed4 c = tex2D(_MainTex, IN.uv_MainTex) * _Color;
	fixed4 d = tex2D(_MKGlowTex, IN.uv_MKGlowTex) * _MKGlowTexColor;
	c += (d * _MKGlowTexStrength);
	
	half rim = 1.0 - saturate(dot(normalize(IN.viewDir), o.Normal));
	o.Emission = _RimColor.rgb * pow(rim, _RimPower);
			
	o.Albedo = c.rgb;
	o.Alpha = c.a;
}
ENDCG
}

Fallback "Transparent/VertexLit"
}
