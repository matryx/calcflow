Shader "MK/MKGlow/Lightmapped/DiffuseBumped" 
{
	Properties 
	{
		_Color ("Main Color", Color) = (1,1,1,1)
		_MainTex ("Base (RGB)", 2D) = "white" {}
		_BumpMap ("Normalmap", 2D) = "bump" {}
		_LightMap ("Lightmap (RGB)", 2D) = "black" {}
		
		_MKGlowColor ("Glow Color", Color) = (1,1,1,1)
		_MKGlowPower ("Glow Power", Range(0.0,5.0)) = 2.5
		_MKGlowTex ("Glow Texture", 2D) = "black" {}
		_MKGlowTexColor ("Glow Texture Color", Color) = (1,1,1,1)
		_MKGlowTexStrength ("Glow Texture Strength ", Range(0.0,1.0)) = 1.0
		_MKGlowOffSet ("Glow Width ", Range(0.0,0.075)) = 0.0
	}

	SubShader 
	{
		LOD 300
		Tags { "RenderType" = "Opaque" }
		CGPROGRAM
		#pragma surface surf Lambert
		struct Input 
		{
		  float2 uv_MainTex;
			float2 uv_MKGlowTex;
		  float2 uv_BumpMap;
		  float2 uv2_LightMap;
		};
		sampler2D _MainTex;
		sampler2D _BumpMap;
		sampler2D _LightMap;
		fixed4 _Color;
		
		sampler2D _MKGlowTex;
		half _MKGlowTexStrength;
		fixed4 _MKGlowTexColor;
		
		void surf (Input IN, inout SurfaceOutput o)
		{
		  o.Albedo = tex2D (_MainTex, IN.uv_MainTex).rgb * _Color;
		  fixed4 d = tex2D(_MKGlowTex, IN.uv_MKGlowTex) * _MKGlowTexColor;
		  o.Albedo += (d * _MKGlowTexStrength);
		  half4 lm = tex2D (_LightMap, IN.uv2_LightMap);
		  o.Emission = lm.rgb*o.Albedo.rgb;
		  o.Alpha = lm.a * _Color.a;
		  o.Normal = UnpackNormal(tex2D(_BumpMap, IN.uv_BumpMap));
		}
		ENDCG
	}
	FallBack "Legacy Shaders/Lightmapped/Diffuse"
}
