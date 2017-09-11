Shader "MK/MKGlow/Mobile/DiffuseBumpSpec" 
{
	Properties 
	{
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
		LOD 250
		
		CGPROGRAM
		#pragma surface surf MobileBlinnPhong exclude_path:prepass nolightmap noforwardadd halfasview

		inline fixed4 LightingMobileBlinnPhong (SurfaceOutput s, fixed3 lightDir, fixed3 halfDir, fixed atten)
		{
			fixed diff = max (0, dot (s.Normal, lightDir));
			fixed nh = max (0, dot (s.Normal, halfDir));
			fixed spec = pow (nh, s.Specular*128) * s.Gloss;
			
			fixed4 c;
			c.rgb = (s.Albedo * _LightColor0.rgb * diff + _LightColor0.rgb * spec) * (atten*2);
			c.a = 0.0;
			return c;
		}

		sampler2D _MainTex;
		sampler2D _BumpMap;
		half _Shininess;
		
		sampler2D _MKGlowTex;
		half _MKGlowTexStrength;
		fixed4 _MKGlowTexColor;

		struct Input 
		{
			float2 uv_MainTex;
			float2 uv_MKGlowTex;
		};

		void surf (Input IN, inout SurfaceOutput o) 
		{
			fixed4 tex = tex2D(_MainTex, IN.uv_MainTex);
			fixed4 d = tex2D(_MKGlowTex, IN.uv_MKGlowTex) * _MKGlowTexColor;
			tex += (d * _MKGlowTexStrength);
			o.Albedo = tex.rgb;
			o.Gloss = tex.a;
			o.Alpha = tex.a;
			o.Specular = _Shininess;
			o.Normal = UnpackNormal (tex2D(_BumpMap, IN.uv_MainTex));
		}
		ENDCG
	}
	FallBack "Mobile/VertexLit"
}
