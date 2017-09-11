Shader "MK/MKGlow/Normal/DiffuseBumpSpec" 
{
	Properties 
	{
		_Color ("Main Color", Color) = (1,1,1,1)
		_SpecColor ("Specular Color", Color) = (0.5, 0.5, 0.5, 1)
		_Shininess ("Shininess", Range (0.03, 1)) = 0.078125
		_MainTex ("Base (RGB) Gloss (A)", 2D) = "white" {}
		_BumpMap ("Normalmap", 2D) = "bump" {}
		
		_MKGlowColor ("Glow Color", Color) = (1,1,1,1)
		_MKGlowPower ("Glow Power", Range(0.0,5.0)) = 2.5
		_MKGlowTex ("Glow Texture", 2D) = "black" {}
		_MKGlowTexColor ("Glow Texture Color", Color) = (1,1,1,1)
		_MKGlowTexStrength ("Glow Texture Strength ", Range(0.0,1.0)) = 1.0
		_MKGlowOffSet ("Glow Width ", Range(0.0,0.075)) = 0.0
	}
	SubShader 
	{ 
		Tags { "RenderType"="MKGlow" }
		LOD 400
		
		CGPROGRAM
		#pragma surface surf BlinnPhong


		sampler2D _MainTex;
		sampler2D _BumpMap;
		fixed4 _Color;
		half _Shininess;
		
		sampler2D _MKGlowTex;
		half _MKGlowTexStrength;
		fixed4 _MKGlowTexColor;

		struct Input 
		{
			float2 uv_MainTex;
			float2 uv_MKGlowTex;
			float2 uv_BumpMap;
		};

		void surf (Input IN, inout SurfaceOutput o) 
		{
			fixed4 tex = tex2D(_MainTex, IN.uv_MainTex);
			fixed4 d = tex2D(_MKGlowTex, IN.uv_MKGlowTex) * _MKGlowTexColor;
			tex += (d * _MKGlowTexStrength);
			o.Albedo = tex.rgb * _Color.rgb;
			o.Gloss = tex.a;
			o.Alpha = tex.a * _Color.a;
			o.Specular = _Shininess;
			o.Normal = UnpackNormal(tex2D(_BumpMap, IN.uv_BumpMap));
		}
		ENDCG
	}
	FallBack "Specular"
}

