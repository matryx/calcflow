using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public interface ExpressionTabInterface
{
    ExpressionSet GetExpSet();
    Transform GetExpressionX();
    Transform GetSeparator();

    void DisableExpression_UI();
    void EnableExpression_UI();
    void DisableActionButtons_UI();

    void SetExpressionX(Transform e);
    void SetSeparator(Transform sep);
    void DeleteExpressionFromScroll();
    bool GetActiveStatus();
}
