//using UnityEngine;
//using System.Collections;

//public class HandAnimationSubScene : VectorFieldInteraction {

//    [SerializeField]
//    Animator lhAnim, rhAnim;

//    [SerializeField]
//    public SteamVR_TrackedObject left, right;

//    private const string AnimLayerNamePoint = "Point Layer";
//    private const string AnimLayerNameThumb = "Thumb Layer";
//    private const string AnimParamNameFlex = "Flex";
//    private const string AnimParamNamePose = "Pose";
//    private int animLayerIndexThumb_left = -1;
//    private int animLayerIndexPoint_left = -1;
//    private int animParamIndexFlex_left = -1;
//    private int animParamIndexPose_left = -1;
//    private float leftFlex = 0.0f;
//    private float leftPoint = 0.0f;
//    private float leftThumbsUp = 0.0f;
//    private int animLayerIndexThumb_right = -1;
//    private int animLayerIndexPoint_right = -1;
//    private int animParamIndexFlex_right = -1;
//    private int animParamIndexPose_right = -1;
//    private float rightFlex = 0.0f;
//    private float rightPoint = 0.0f;
//    private float rightThumbsUp = 0.0f;

//    protected override void OnEnable()
//    {
//        base.OnEnable();
//        lhAnim.gameObject.transform.parent.gameObject.SetActive(true);
//        rhAnim.gameObject.transform.parent.gameObject.SetActive(true);
//    }

//    protected override void OnDisable()
//    {
//        base.OnDisable();
//        lhAnim.gameObject.transform.parent.gameObject.SetActive(false);
//        rhAnim.gameObject.transform.parent.gameObject.SetActive(false);
//    }

//    // Use this for initialization
//    protected override void Start()
//    {
//        base.Start();

//        left = controllers.GetComponent<SteamVR_ControllerManager>().left.GetComponent<SteamVR_TrackedObject>();
//        right = controllers.GetComponent<SteamVR_ControllerManager>().right.GetComponent<SteamVR_TrackedObject>();

//        animLayerIndexPoint_left = lhAnim.GetLayerIndex(AnimLayerNamePoint);
//        animLayerIndexThumb_left = lhAnim.GetLayerIndex(AnimLayerNameThumb);
//        animParamIndexFlex_left = Animator.StringToHash(AnimParamNameFlex);
//        animParamIndexPose_left = Animator.StringToHash(AnimParamNamePose);

//        animLayerIndexPoint_right = rhAnim.GetLayerIndex(AnimLayerNamePoint);
//        animLayerIndexThumb_right = rhAnim.GetLayerIndex(AnimLayerNameThumb);
//        animParamIndexFlex_right = Animator.StringToHash(AnimParamNameFlex);
//        animParamIndexPose_right = Animator.StringToHash(AnimParamNamePose);

//    }

//    private float InputLeftFlex()
//    {
//        return SteamVR_Controller.Input((int)left.index).GetAxis(Valve.VR.EVRButtonId.k_EButton_SteamVR_Trigger).x;
//    }
//    private bool InputLeftPoint()
//    {
//        return SteamVR_Controller.Input((int)left.index).GetPress(SteamVR_Controller.ButtonMask.Grip);
//    }

//    private float InputRightFlex()
//    {
//        return SteamVR_Controller.Input((int)right.index).GetAxis(Valve.VR.EVRButtonId.k_EButton_SteamVR_Trigger).x;
//    }
//    private bool InputRightPoint()
//    {
//        return SteamVR_Controller.Input((int)right.index).GetPress(SteamVR_Controller.ButtonMask.Grip);
//    }

//    private float InputValueRateChange(bool isDown, float value)
//    {
//        float rateDelta = Time.deltaTime * 20f;
//        float sign = isDown ? 1.0f : -1.0f;
//        return Mathf.Clamp01(value + rateDelta * sign);
//    }

//    // Update is called once per frame
//    protected override void Update()
//    {
//        animLayerIndexPoint_left = lhAnim.GetLayerIndex(AnimLayerNamePoint);
//        animLayerIndexPoint_right = rhAnim.GetLayerIndex(AnimLayerNamePoint);
//        //leftFlex = InputValueRateChange(InputFlex(), leftFlex);
//        leftFlex = InputLeftFlex();
//        leftPoint = InputValueRateChange(InputLeftPoint(), leftPoint);
//        rightFlex = InputRightFlex();
//        rightPoint = InputValueRateChange(InputRightPoint(), rightPoint);

//        //anim.SetInteger(animParamIndexPose, 0);
//        lhAnim.SetFloat(animParamIndexFlex_left, leftFlex);
//        lhAnim.SetInteger(animParamIndexPose_left, (leftActive == null) ? 0 : 1);
//        lhAnim.SetLayerWeight(animLayerIndexPoint_left, leftPoint);
//        rhAnim.SetFloat(animParamIndexFlex_right, rightFlex);
//        rhAnim.SetInteger(animParamIndexPose_right, (rightActive == null) ? 0 : 1);
//        rhAnim.SetLayerWeight(animLayerIndexPoint_right, rightPoint);
//    }
//}
