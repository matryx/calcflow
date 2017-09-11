// Upgrade NOTE: replaced 'mul(UNITY_MATRIX_MVP,*)' with 'UnityObjectToClipPos(*)'

Shader "MK/MKGlow/Cutout/SoftEdgeUnlit" {
Properties {
	_Color ("Main Color", Color) = (1, 1, 1, 1)
	_MainTex ("Base (RGB) Alpha (A)", 2D) = "white" {}
	_Cutoff ("Base Alpha cutoff", Range (0,.9)) = .5
	
	_MKGlowColor ("Glow Color", Color) = (1,1,1,1)
	_MKGlowPower ("Glow Power", Range(0.0,5.0)) = 2.5
	_MKGlowTex ("Glow Texture", 2D) = "black" {}
	_MKGlowTexColor ("Glow Texture Color", Color) = (1,1,1,1)
	_MKGlowTexStrength ("Glow Texture Strength ", Range(0.0,1.0)) = 1.0
	_MKGlowOffSet ("Glow Width ", Range(0.0,0.15)) = 0.0
}

SubShader {
	Tags { "Queue"="AlphaTest" "IgnoreProjector"="True" "RenderType"="MKGlow" }
	Lighting off
	
	// Render both front and back facing polygons.
	Cull Off
	
	// first pass:
	//   render any pixels that are more than [_Cutoff] opaque
	Pass {  
		CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			
			#include "UnityCG.cginc"

			struct appdata_t {
				float4 vertex : POSITION;
				float4 color : COLOR;
				float2 texcoord : TEXCOORD0;
			};

			struct v2f {
				float4 vertex : SV_POSITION;
				float4 color : COLOR;
				float2 texcoord : TEXCOORD0;
			};

			sampler2D _MainTex;
			float4 _MainTex_ST;
			float _Cutoff;
			
			sampler2D _MKGlowTex;
			half _MKGlowTexStrength;
			fixed4 _MKGlowTexColor;
			
			v2f vert (appdata_t v)
			{
				v2f o;
				o.vertex = UnityObjectToClipPos(v.vertex);
				o.color = v.color;
				o.texcoord = TRANSFORM_TEX(v.texcoord, _MainTex);
				return o;
			}
			
			float4 _Color;
			half4 frag (v2f i) : SV_Target
			{
				half4 col = _Color * tex2D(_MainTex, i.texcoord);
				fixed4 d = tex2D(_MKGlowTex, i.texcoord) * _MKGlowTexColor;
				col += (d * _MKGlowTexStrength);
				clip(col.a - _Cutoff);
				return col;
			}
		ENDCG
	}

	// Second pass:
	//   render the semitransparent details.
	Pass {
		Tags { "RequireOption" = "SoftVegetation" }
		
		// Dont write to the depth buffer
		ZWrite off
		
		// Set up alpha blending
		Blend SrcAlpha OneMinusSrcAlpha
		
		CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			
			#include "UnityCG.cginc"

			struct appdata_t {
				float4 vertex : POSITION;
				float4 color : COLOR;
				float2 texcoord : TEXCOORD0;
			};

			struct v2f {
				float4 vertex : SV_POSITION;
				float4 color : COLOR;
				float2 texcoord : TEXCOORD0;
			};

			sampler2D _MainTex;
			float4 _MainTex_ST;
			float _Cutoff;
			
			v2f vert (appdata_t v)
			{
				v2f o;
				o.vertex = UnityObjectToClipPos(v.vertex);
				o.color = v.color;
				o.texcoord = TRANSFORM_TEX(v.texcoord, _MainTex);
				return o;
			}
			
			float4 _Color;
			half4 frag (v2f i) : SV_Target
			{
				half4 col = _Color * tex2D(_MainTex, i.texcoord);
				clip(-(col.a - _Cutoff));
				return col;
			}
		ENDCG
	}
}

}
