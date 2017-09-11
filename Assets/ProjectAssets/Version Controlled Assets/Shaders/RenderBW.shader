// Upgrade NOTE: replaced 'mul(UNITY_MATRIX_MVP,*)' with 'UnityObjectToClipPos(*)'

Shader "Custom/RenderBW" {
	SubShader{
		ZWrite Off
		ZTest Always
		Lighting Off
		Pass {
			CGPROGRAM
#pragma vertex vert
#pragma fragment frag
#include "UnityCG.cginc"
			struct v2f {
				float4 pos : POSITION;
			};
			v2f vert(v2f vin) 
			{
				v2f o;
				o.pos = UnityObjectToClipPos(vin.pos);
				return o;
			}
			half4 frag() : COLOR0
			{
				// render objects to pure white on the target
				return half4(1,1,1,1);
			}
			ENDCG
		}
	}
}
