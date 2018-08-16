Shader "RaymarchedDensity"
{
	Properties
	{
		_VolumeTex ("Texture", 3D) = "white" {}
		_Scale ("Scale", Float) = 1
		_MaxSteps("Step Limit", Float)=64
	}
	SubShader
	{
		Tags { "Queue"="Transparent" "RenderType"="Transparent" }
		LOD 100
		ZWrite Off
		Blend SrcAlpha OneMinusSrcAlpha

		Pass
		{
			CGPROGRAM
			#pragma vertex vert
			#pragma fragment frag
			// make fog work
			#pragma multi_compile_fog
			
			#include "UnityCG.cginc"

			struct appdata
			{
				float4 vertex : POSITION;
			};

			struct v2f
			{
				float4 vertex : SV_POSITION;
				//float3 uvw : TEXCOORD0;
				float4 worldPos : TEXCOORD1;
				float4 localPos : TEXCOORD2;
			};

			sampler3D _VolumeTex;
			float4 _MainTex_ST;
			float _Scale;
			int _MaxSteps;

/*
			float inCube(float length, float3 currLoc){
				float3 c = float3(length, length, length);
				float3 d = abs(currLoc) - c;
				return max(max(d.x,max(d.y, d.z)), 0);
			}
*/
			// Returns 0 if currLoc is inside the sphere; Returns a positive number otherwise.
			bool inSphere(float rad, float3 currLoc){
				return bool(max(length(currLoc)-rad, 0));
			}

			// Samples the 3D texture
			inline fixed4 getTexColor (float3 uvw){
				return tex3D(_VolumeTex, uvw);
			}

			// Raymarch at a fixed interval of .025, blending alpha and color values along the way
			fixed4 raymarchColor(float4 start, float3 dir){
				float rayDepth = 0;
				fixed4 color = fixed4(0,0,0,0);
				fixed4 tempColor = fixed4(0,0,0,0);
				int shifter = 0;
				[loop]
				for (int i = 0; i < _MaxSteps; i++){
					// if(!inCube(_Scale, start+rayDepth*dir)){
					// 	break;
					// }
					if(inSphere(_Scale, start.xyz+rayDepth*dir)){
						break;
					}
					tempColor = getTexColor((start.xyz+rayDepth*dir)+.5);
					color.rgb = (1-color.a) * tempColor.rgb + color.rgb;
					color.a = (1-color.a) * tempColor.a + color.a;
					rayDepth += .025;
				}
				return color;
			}

			v2f vert (appdata v)
			{
				v2f o;
				o.vertex = UnityObjectToClipPos(v.vertex);
				o.worldPos = mul(unity_ObjectToWorld, v.vertex);
				o.localPos = v.vertex;
				return o;
			}
			
			fixed4 frag (v2f i) : SV_Target
			{
				float3 worldDir = i.worldPos - _WorldSpaceCameraPos;
				float3 localDir = normalize (mul(unity_WorldToObject, worldDir));
				fixed4 col = raymarchColor(i.localPos, localDir);
				return col;
			}
			ENDCG
		}
	}
}
